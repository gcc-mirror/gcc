------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ T Y P E                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2016, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Alloc;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Nlists;   use Nlists;
with Errout;   use Errout;
with Lib;      use Lib;
with Namet;    use Namet;
with Opt;      use Opt;
with Output;   use Output;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch12; use Sem_Ch12;
with Sem_Disp; use Sem_Disp;
with Sem_Dist; use Sem_Dist;
with Sem_Util; use Sem_Util;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Table;
with Treepr;   use Treepr;
with Uintp;    use Uintp;

package body Sem_Type is

   ---------------------
   -- Data Structures --
   ---------------------

   --  The following data structures establish a mapping between nodes and
   --  their interpretations. An overloaded node has an entry in Interp_Map,
   --  which in turn contains a pointer into the All_Interp array. The
   --  interpretations of a given node are contiguous in All_Interp. Each set
   --  of interpretations is terminated with the marker No_Interp. In order to
   --  speed up the retrieval of the interpretations of an overloaded node, the
   --  Interp_Map table is accessed by means of a simple hashing scheme, and
   --  the entries in Interp_Map are chained. The heads of clash lists are
   --  stored in array Headers.

   --              Headers        Interp_Map          All_Interp

   --                 _            +-----+             +--------+
   --                |_|           |_____|         --->|interp1 |
   --                |_|---------->|node |         |   |interp2 |
   --                |_|           |index|---------|   |nointerp|
   --                |_|           |next |             |        |
   --                              |-----|             |        |
   --                              +-----+             +--------+

   --  This scheme does not currently reclaim interpretations. In principle,
   --  after a unit is compiled, all overloadings have been resolved, and the
   --  candidate interpretations should be deleted. This should be easier
   --  now than with the previous scheme???

   package All_Interp is new Table.Table (
     Table_Component_Type => Interp,
     Table_Index_Type     => Interp_Index,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.All_Interp_Initial,
     Table_Increment      => Alloc.All_Interp_Increment,
     Table_Name           => "All_Interp");

   type Interp_Ref is record
      Node  : Node_Id;
      Index : Interp_Index;
      Next  : Int;
   end record;

   Header_Size : constant Int := 2 ** 12;
   No_Entry    : constant Int := -1;
   Headers     : array (0 .. Header_Size) of Int := (others => No_Entry);

   package Interp_Map is new Table.Table (
     Table_Component_Type => Interp_Ref,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.Interp_Map_Initial,
     Table_Increment      => Alloc.Interp_Map_Increment,
     Table_Name           => "Interp_Map");

   function Hash (N : Node_Id) return Int;
   --  A trivial hashing function for nodes, used to insert an overloaded
   --  node into the Interp_Map table.

   -------------------------------------
   -- Handling of Overload Resolution --
   -------------------------------------

   --  Overload resolution uses two passes over the syntax tree of a complete
   --  context. In the first, bottom-up pass, the types of actuals in calls
   --  are used to resolve possibly overloaded subprogram and operator names.
   --  In the second top-down pass, the type of the context (for example the
   --  condition in a while statement) is used to resolve a possibly ambiguous
   --  call, and the unique subprogram name in turn imposes a specific context
   --  on each of its actuals.

   --  Most expressions are in fact unambiguous, and the bottom-up pass is
   --  sufficient  to resolve most everything. To simplify the common case,
   --  names and expressions carry a flag Is_Overloaded to indicate whether
   --  they have more than one interpretation. If the flag is off, then each
   --  name has already a unique meaning and type, and the bottom-up pass is
   --  sufficient (and much simpler).

   --------------------------
   -- Operator Overloading --
   --------------------------

   --  The visibility of operators is handled differently from that of other
   --  entities. We do not introduce explicit versions of primitive operators
   --  for each type definition. As a result, there is only one entity
   --  corresponding to predefined addition on all numeric types, etc. The
   --  back end resolves predefined operators according to their type. The
   --  visibility of primitive operations then reduces to the visibility of the
   --  resulting type: (a + b) is a legal interpretation of some primitive
   --  operator + if the type of the result (which must also be the type of a
   --  and b) is directly visible (either immediately visible or use-visible).

   --  User-defined operators are treated like other functions, but the
   --  visibility of these user-defined operations must be special-cased
   --  to determine whether they hide or are hidden by predefined operators.
   --  The form P."+" (x, y) requires additional handling.

   --  Concatenation is treated more conventionally: for every one-dimensional
   --  array type we introduce a explicit concatenation operator. This is
   --  necessary to handle the case of (element & element => array) which
   --  cannot be handled conveniently if there is no explicit instance of
   --  resulting type of the operation.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure All_Overloads;
   pragma Warnings (Off, All_Overloads);
   --  Debugging procedure: list full contents of Overloads table

   function Binary_Op_Interp_Has_Abstract_Op
     (N : Node_Id;
      E : Entity_Id) return Entity_Id;
   --  Given the node and entity of a binary operator, determine whether the
   --  actuals of E contain an abstract interpretation with regards to the
   --  types of their corresponding formals. Return the abstract operation or
   --  Empty.

   function Function_Interp_Has_Abstract_Op
     (N : Node_Id;
      E : Entity_Id) return Entity_Id;
   --  Given the node and entity of a function call, determine whether the
   --  actuals of E contain an abstract interpretation with regards to the
   --  types of their corresponding formals. Return the abstract operation or
   --  Empty.

   function Has_Abstract_Op
     (N   : Node_Id;
      Typ : Entity_Id) return Entity_Id;
   --  Subsidiary routine to Binary_Op_Interp_Has_Abstract_Op and Function_
   --  Interp_Has_Abstract_Op. Determine whether an overloaded node has an
   --  abstract interpretation which yields type Typ.

   procedure New_Interps (N : Node_Id);
   --  Initialize collection of interpretations for the given node, which is
   --  either an overloaded entity, or an operation whose arguments have
   --  multiple interpretations. Interpretations can be added to only one
   --  node at a time.

   function Specific_Type (Typ_1, Typ_2 : Entity_Id) return Entity_Id;
   --  If Typ_1 and Typ_2 are compatible, return the one that is not universal
   --  or is not a "class" type (any_character, etc).

   --------------------
   -- Add_One_Interp --
   --------------------

   procedure Add_One_Interp
     (N         : Node_Id;
      E         : Entity_Id;
      T         : Entity_Id;
      Opnd_Type : Entity_Id := Empty)
   is
      Vis_Type : Entity_Id;

      procedure Add_Entry (Name : Entity_Id; Typ : Entity_Id);
      --  Add one interpretation to an overloaded node. Add a new entry if
      --  not hidden by previous one, and remove previous one if hidden by
      --  new one.

      function Is_Universal_Operation (Op : Entity_Id) return Boolean;
      --  True if the entity is a predefined operator and the operands have
      --  a universal Interpretation.

      ---------------
      -- Add_Entry --
      ---------------

      procedure Add_Entry (Name : Entity_Id; Typ : Entity_Id) is
         Abstr_Op : Entity_Id := Empty;
         I        : Interp_Index;
         It       : Interp;

      --  Start of processing for Add_Entry

      begin
         --  Find out whether the new entry references interpretations that
         --  are abstract or disabled by abstract operators.

         if Ada_Version >= Ada_2005 then
            if Nkind (N) in N_Binary_Op then
               Abstr_Op := Binary_Op_Interp_Has_Abstract_Op (N, Name);
            elsif Nkind (N) = N_Function_Call then
               Abstr_Op := Function_Interp_Has_Abstract_Op (N, Name);
            end if;
         end if;

         Get_First_Interp (N, I, It);
         while Present (It.Nam) loop

            --  A user-defined subprogram hides another declared at an outer
            --  level, or one that is use-visible. So return if previous
            --  definition hides new one (which is either in an outer
            --  scope, or use-visible). Note that for functions use-visible
            --  is the same as potentially use-visible. If new one hides
            --  previous one, replace entry in table of interpretations.
            --  If this is a universal operation, retain the operator in case
            --  preference rule applies.

            if (((Ekind (Name) = E_Function or else Ekind (Name) = E_Procedure)
                   and then Ekind (Name) = Ekind (It.Nam))
                 or else (Ekind (Name) = E_Operator
                           and then Ekind (It.Nam) = E_Function))
              and then Is_Immediately_Visible (It.Nam)
              and then Type_Conformant (Name, It.Nam)
              and then Base_Type (It.Typ) = Base_Type (T)
            then
               if Is_Universal_Operation (Name) then
                  exit;

               --  If node is an operator symbol, we have no actuals with
               --  which to check hiding, and this is done in full in the
               --  caller (Analyze_Subprogram_Renaming) so we include the
               --  predefined operator in any case.

               elsif Nkind (N) = N_Operator_Symbol
                 or else
                   (Nkind (N) = N_Expanded_Name
                     and then Nkind (Selector_Name (N)) = N_Operator_Symbol)
               then
                  exit;

               elsif not In_Open_Scopes (Scope (Name))
                 or else Scope_Depth (Scope (Name)) <=
                         Scope_Depth (Scope (It.Nam))
               then
                  --  If ambiguity within instance, and entity is not an
                  --  implicit operation, save for later disambiguation.

                  if Scope (Name) = Scope (It.Nam)
                    and then not Is_Inherited_Operation (Name)
                    and then In_Instance
                  then
                     exit;
                  else
                     return;
                  end if;

               else
                  All_Interp.Table (I).Nam := Name;
                  return;
               end if;

            --  Avoid making duplicate entries in overloads

            elsif Name = It.Nam
              and then Base_Type (It.Typ) = Base_Type (T)
            then
               return;

            --  Otherwise keep going

            else
               Get_Next_Interp (I, It);
            end if;

         end loop;

         All_Interp.Table (All_Interp.Last) := (Name, Typ, Abstr_Op);
         All_Interp.Append (No_Interp);
      end Add_Entry;

      ----------------------------
      -- Is_Universal_Operation --
      ----------------------------

      function Is_Universal_Operation (Op : Entity_Id) return Boolean is
         Arg : Node_Id;

      begin
         if Ekind (Op) /= E_Operator then
            return False;

         elsif Nkind (N) in N_Binary_Op then
            return Present (Universal_Interpretation (Left_Opnd (N)))
              and then Present (Universal_Interpretation (Right_Opnd (N)));

         elsif Nkind (N) in N_Unary_Op then
            return Present (Universal_Interpretation (Right_Opnd (N)));

         elsif Nkind (N) = N_Function_Call then
            Arg := First_Actual (N);
            while Present (Arg) loop
               if No (Universal_Interpretation (Arg)) then
                  return False;
               end if;

               Next_Actual (Arg);
            end loop;

            return True;

         else
            return False;
         end if;
      end Is_Universal_Operation;

   --  Start of processing for Add_One_Interp

   begin
      --  If the interpretation is a predefined operator, verify that the
      --  result type is visible, or that the entity has already been
      --  resolved (case of an instantiation node that refers to a predefined
      --  operation, or an internally generated operator node, or an operator
      --  given as an expanded name). If the operator is a comparison or
      --  equality, it is the type of the operand that matters to determine
      --  whether the operator is visible. In an instance, the check is not
      --  performed, given that the operator was visible in the generic.

      if Ekind (E) = E_Operator then
         if Present (Opnd_Type) then
            Vis_Type := Opnd_Type;
         else
            Vis_Type := Base_Type (T);
         end if;

         if In_Open_Scopes (Scope (Vis_Type))
           or else Is_Potentially_Use_Visible (Vis_Type)
           or else In_Use (Vis_Type)
           or else (In_Use (Scope (Vis_Type))
                     and then not Is_Hidden (Vis_Type))
           or else Nkind (N) = N_Expanded_Name
           or else (Nkind (N) in N_Op and then E = Entity (N))
           or else In_Instance
           or else Ekind (Vis_Type) = E_Anonymous_Access_Type
         then
            null;

         --  If the node is given in functional notation and the prefix
         --  is an expanded name, then the operator is visible if the
         --  prefix is the scope of the result type as well. If the
         --  operator is (implicitly) defined in an extension of system,
         --  it is know to be valid (see Defined_In_Scope, sem_ch4.adb).

         elsif Nkind (N) = N_Function_Call
           and then Nkind (Name (N)) = N_Expanded_Name
           and then (Entity (Prefix (Name (N))) = Scope (Base_Type (T))
                      or else Entity (Prefix (Name (N))) = Scope (Vis_Type)
                      or else Scope (Vis_Type) = System_Aux_Id)
         then
            null;

         --  Save type for subsequent error message, in case no other
         --  interpretation is found.

         else
            Candidate_Type := Vis_Type;
            return;
         end if;

      --  In an instance, an abstract non-dispatching operation cannot be a
      --  candidate interpretation, because it could not have been one in the
      --  generic (it may be a spurious overloading in the instance).

      elsif In_Instance
        and then Is_Overloadable (E)
        and then Is_Abstract_Subprogram (E)
        and then not Is_Dispatching_Operation (E)
      then
         return;

      --  An inherited interface operation that is implemented by some derived
      --  type does not participate in overload resolution, only the
      --  implementation operation does.

      elsif Is_Hidden (E)
        and then Is_Subprogram (E)
        and then Present (Interface_Alias (E))
      then
         --  Ada 2005 (AI-251): If this primitive operation corresponds with
         --  an immediate ancestor interface there is no need to add it to the
         --  list of interpretations. The corresponding aliased primitive is
         --  also in this list of primitive operations and will be used instead
         --  because otherwise we have a dummy ambiguity between the two
         --  subprograms which are in fact the same.

         if not Is_Ancestor
                  (Find_Dispatching_Type (Interface_Alias (E)),
                   Find_Dispatching_Type (E))
         then
            Add_One_Interp (N, Interface_Alias (E), T);
         end if;

         return;

      --  Calling stubs for an RACW operation never participate in resolution,
      --  they are executed only through dispatching calls.

      elsif Is_RACW_Stub_Type_Operation (E) then
         return;
      end if;

      --  If this is the first interpretation of N, N has type Any_Type.
      --  In that case place the new type on the node. If one interpretation
      --  already exists, indicate that the node is overloaded, and store
      --  both the previous and the new interpretation in All_Interp. If
      --  this is a later interpretation, just add it to the set.

      if Etype (N) = Any_Type then
         if Is_Type (E) then
            Set_Etype (N, T);

         else
            --  Record both the operator or subprogram name, and its type

            if Nkind (N) in N_Op or else Is_Entity_Name (N) then
               Set_Entity (N, E);
            end if;

            Set_Etype (N, T);
         end if;

      --  Either there is no current interpretation in the table for any
      --  node or the interpretation that is present is for a different
      --  node. In both cases add a new interpretation to the table.

      elsif Interp_Map.Last < 0
        or else
          (Interp_Map.Table (Interp_Map.Last).Node /= N
            and then not Is_Overloaded (N))
      then
         New_Interps (N);

         if (Nkind (N) in N_Op or else Is_Entity_Name (N))
           and then Present (Entity (N))
         then
            Add_Entry (Entity (N), Etype (N));

         elsif Nkind (N) in N_Subprogram_Call
           and then Is_Entity_Name (Name (N))
         then
            Add_Entry (Entity (Name (N)), Etype (N));

         --  If this is an indirect call there will be no name associated
         --  with the previous entry. To make diagnostics clearer, save
         --  Subprogram_Type of first interpretation, so that the error will
         --  point to the anonymous access to subprogram, not to the result
         --  type of the call itself.

         elsif (Nkind (N)) = N_Function_Call
           and then Nkind (Name (N)) = N_Explicit_Dereference
           and then Is_Overloaded (Name (N))
         then
            declare
               It : Interp;

               Itn : Interp_Index;
               pragma Warnings (Off, Itn);

            begin
               Get_First_Interp (Name (N), Itn, It);
               Add_Entry (It.Nam, Etype (N));
            end;

         else
            --  Overloaded prefix in indexed or selected component, or call
            --  whose name is an expression or another call.

            Add_Entry (Etype (N), Etype (N));
         end if;

         Add_Entry (E, T);

      else
         Add_Entry (E, T);
      end if;
   end Add_One_Interp;

   -------------------
   -- All_Overloads --
   -------------------

   procedure All_Overloads is
   begin
      for J in All_Interp.First .. All_Interp.Last loop

         if Present (All_Interp.Table (J).Nam) then
            Write_Entity_Info (All_Interp.Table (J). Nam, " ");
         else
            Write_Str ("No Interp");
            Write_Eol;
         end if;

         Write_Str ("=================");
         Write_Eol;
      end loop;
   end All_Overloads;

   --------------------------------------
   -- Binary_Op_Interp_Has_Abstract_Op --
   --------------------------------------

   function Binary_Op_Interp_Has_Abstract_Op
     (N : Node_Id;
      E : Entity_Id) return Entity_Id
   is
      Abstr_Op : Entity_Id;
      E_Left   : constant Node_Id := First_Formal (E);
      E_Right  : constant Node_Id := Next_Formal (E_Left);

   begin
      Abstr_Op := Has_Abstract_Op (Left_Opnd (N), Etype (E_Left));
      if Present (Abstr_Op) then
         return Abstr_Op;
      end if;

      return Has_Abstract_Op (Right_Opnd (N), Etype (E_Right));
   end Binary_Op_Interp_Has_Abstract_Op;

   ---------------------
   -- Collect_Interps --
   ---------------------

   procedure Collect_Interps (N : Node_Id) is
      Ent          : constant Entity_Id := Entity (N);
      H            : Entity_Id;
      First_Interp : Interp_Index;

      function Within_Instance (E : Entity_Id) return Boolean;
      --  Within an instance there can be spurious ambiguities between a local
      --  entity and one declared outside of the instance. This can only happen
      --  for subprograms, because otherwise the local entity hides the outer
      --  one. For an overloadable entity, this predicate determines whether it
      --  is a candidate within the instance, or must be ignored.

      ---------------------
      -- Within_Instance --
      ---------------------

      function Within_Instance (E : Entity_Id) return Boolean is
         Inst : Entity_Id;
         Scop : Entity_Id;

      begin
         if not In_Instance then
            return False;
         end if;

         Inst := Current_Scope;
         while Present (Inst) and then not Is_Generic_Instance (Inst) loop
            Inst := Scope (Inst);
         end loop;

         Scop := Scope (E);
         while Present (Scop) and then Scop /= Standard_Standard loop
            if Scop = Inst then
               return True;
            end if;

            Scop := Scope (Scop);
         end loop;

         return False;
      end Within_Instance;

   --  Start of processing for Collect_Interps

   begin
      New_Interps (N);

      --  Unconditionally add the entity that was initially matched

      First_Interp := All_Interp.Last;
      Add_One_Interp (N, Ent, Etype (N));

      --  For expanded name, pick up all additional entities from the
      --  same scope, since these are obviously also visible. Note that
      --  these are not necessarily contiguous on the homonym chain.

      if Nkind (N) = N_Expanded_Name then
         H := Homonym (Ent);
         while Present (H) loop
            if Scope (H) = Scope (Entity (N)) then
               Add_One_Interp (N, H, Etype (H));
            end if;

            H := Homonym (H);
         end loop;

      --  Case of direct name

      else
         --  First, search the homonym chain for directly visible entities

         H := Current_Entity (Ent);
         while Present (H) loop
            exit when
              not Is_Overloadable (H)
                and then Is_Immediately_Visible (H);

            if Is_Immediately_Visible (H) and then H /= Ent then

               --  Only add interpretation if not hidden by an inner
               --  immediately visible one.

               for J in First_Interp .. All_Interp.Last - 1 loop

                  --  Current homograph is not hidden. Add to overloads

                  if not Is_Immediately_Visible (All_Interp.Table (J).Nam) then
                     exit;

                  --  Homograph is hidden, unless it is a predefined operator

                  elsif Type_Conformant (H, All_Interp.Table (J).Nam) then

                     --  A homograph in the same scope can occur within an
                     --  instantiation, the resulting ambiguity has to be
                     --  resolved later. The homographs may both be local
                     --  functions or actuals, or may be declared at different
                     --  levels within the instance. The renaming of an actual
                     --  within the instance must not be included.

                     if Within_Instance (H)
                       and then H /= Renamed_Entity (Ent)
                       and then not Is_Inherited_Operation (H)
                     then
                        All_Interp.Table (All_Interp.Last) :=
                          (H, Etype (H), Empty);
                        All_Interp.Append (No_Interp);
                        goto Next_Homograph;

                     elsif Scope (H) /= Standard_Standard then
                        goto Next_Homograph;
                     end if;
                  end if;
               end loop;

               --  On exit, we know that current homograph is not hidden

               Add_One_Interp (N, H, Etype (H));

               if Debug_Flag_E then
                  Write_Str ("Add overloaded interpretation ");
                  Write_Int (Int (H));
                  Write_Eol;
               end if;
            end if;

            <<Next_Homograph>>
               H := Homonym (H);
         end loop;

         --  Scan list of homographs for use-visible entities only

         H := Current_Entity (Ent);

         while Present (H) loop
            if Is_Potentially_Use_Visible (H)
              and then H /= Ent
              and then Is_Overloadable (H)
            then
               for J in First_Interp .. All_Interp.Last - 1 loop

                  if not Is_Immediately_Visible (All_Interp.Table (J).Nam) then
                     exit;

                  elsif Type_Conformant (H, All_Interp.Table (J).Nam) then
                     goto Next_Use_Homograph;
                  end if;
               end loop;

               Add_One_Interp (N, H, Etype (H));
            end if;

            <<Next_Use_Homograph>>
               H := Homonym (H);
         end loop;
      end if;

      if All_Interp.Last = First_Interp + 1 then

         --  The final interpretation is in fact not overloaded. Note that the
         --  unique legal interpretation may or may not be the original one,
         --  so we need to update N's entity and etype now, because once N
         --  is marked as not overloaded it is also expected to carry the
         --  proper interpretation.

         Set_Is_Overloaded (N, False);
         Set_Entity (N, All_Interp.Table (First_Interp).Nam);
         Set_Etype  (N, All_Interp.Table (First_Interp).Typ);
      end if;
   end Collect_Interps;

   ------------
   -- Covers --
   ------------

   function Covers (T1, T2 : Entity_Id) return Boolean is
      BT1 : Entity_Id;
      BT2 : Entity_Id;

      function Full_View_Covers (Typ1, Typ2 : Entity_Id) return Boolean;
      --  In an instance the proper view may not always be correct for
      --  private types, but private and full view are compatible. This
      --  removes spurious errors from nested instantiations that involve,
      --  among other things, types derived from private types.

      function Real_Actual (T : Entity_Id) return Entity_Id;
      --  If an actual in an inner instance is the formal of an enclosing
      --  generic, the actual in the enclosing instance is the one that can
      --  create an accidental ambiguity, and the check on compatibily of
      --  generic actual types must use this enclosing actual.

      ----------------------
      -- Full_View_Covers --
      ----------------------

      function Full_View_Covers (Typ1, Typ2 : Entity_Id) return Boolean is
      begin
         return
           Is_Private_Type (Typ1)
             and then
              ((Present (Full_View (Typ1))
                 and then Covers (Full_View (Typ1), Typ2))
                or else (Present (Underlying_Full_View (Typ1))
                          and then Covers (Underlying_Full_View (Typ1), Typ2))
                or else Base_Type (Typ1) = Typ2
                or else Base_Type (Typ2) = Typ1);
      end Full_View_Covers;

      -----------------
      -- Real_Actual --
      -----------------

      function Real_Actual (T : Entity_Id) return Entity_Id is
         Par : constant Node_Id := Parent (T);
         RA  : Entity_Id;

      begin
         --  Retrieve parent subtype from subtype declaration for actual

         if Nkind (Par) = N_Subtype_Declaration
           and then not Comes_From_Source (Par)
           and then Is_Entity_Name (Subtype_Indication (Par))
         then
            RA := Entity (Subtype_Indication (Par));

            if Is_Generic_Actual_Type (RA) then
               return RA;
            end if;
         end if;

         --  Otherwise actual is not the actual of an enclosing instance

         return T;
      end Real_Actual;

   --  Start of processing for Covers

   begin
      --  If either operand missing, then this is an error, but ignore it (and
      --  pretend we have a cover) if errors already detected, since this may
      --  simply mean we have malformed trees or a semantic error upstream.

      if No (T1) or else No (T2) then
         if Total_Errors_Detected /= 0 then
            return True;
         else
            raise Program_Error;
         end if;
      end if;

      --  Trivial case: same types are always compatible

      if T1 = T2 then
         return True;
      end if;

      --  First check for Standard_Void_Type, which is special. Subsequent
      --  processing in this routine assumes T1 and T2 are bona fide types;
      --  Standard_Void_Type is a special entity that has some, but not all,
      --  properties of types.

      if (T1 = Standard_Void_Type) /= (T2 = Standard_Void_Type) then
         return False;
      end if;

      BT1 := Base_Type (T1);
      BT2 := Base_Type (T2);

      --  Handle underlying view of records with unknown discriminants
      --  using the original entity that motivated the construction of
      --  this underlying record view (see Build_Derived_Private_Type).

      if Is_Underlying_Record_View (BT1) then
         BT1 := Underlying_Record_View (BT1);
      end if;

      if Is_Underlying_Record_View (BT2) then
         BT2 := Underlying_Record_View (BT2);
      end if;

      --  Simplest case: types that have the same base type and are not generic
      --  actuals are compatible. Generic actuals belong to their class but are
      --  not compatible with other types of their class, and in particular
      --  with other generic actuals. They are however compatible with their
      --  own subtypes, and itypes with the same base are compatible as well.
      --  Similarly, constrained subtypes obtained from expressions of an
      --  unconstrained nominal type are compatible with the base type (may
      --  lead to spurious ambiguities in obscure cases ???)

      --  Generic actuals require special treatment to avoid spurious ambi-
      --  guities in an instance, when two formal types are instantiated with
      --  the same actual, so that different subprograms end up with the same
      --  signature in the instance. If a generic actual is the actual of an
      --  enclosing instance, it is that actual that we must compare: generic
      --  actuals are only incompatible if they appear in the same instance.

      if BT1 = BT2
        or else BT1 = T2
        or else BT2 = T1
      then
         if not Is_Generic_Actual_Type (T1)
              or else
            not Is_Generic_Actual_Type (T2)
         then
            return True;

         --  Both T1 and T2 are generic actual types

         else
            declare
               RT1 : constant Entity_Id := Real_Actual (T1);
               RT2 : constant Entity_Id := Real_Actual (T2);
            begin
               return RT1 = RT2
                  or else Is_Itype (T1)
                  or else Is_Itype (T2)
                  or else Is_Constr_Subt_For_U_Nominal (T1)
                  or else Is_Constr_Subt_For_U_Nominal (T2)
                  or else Scope (RT1) /= Scope (RT2);
            end;
         end if;

      --  Literals are compatible with types in a given "class"

      elsif     (T2 = Universal_Integer and then Is_Integer_Type (T1))
        or else (T2 = Universal_Real    and then Is_Real_Type (T1))
        or else (T2 = Universal_Fixed   and then Is_Fixed_Point_Type (T1))
        or else (T2 = Any_Fixed         and then Is_Fixed_Point_Type (T1))
        or else (T2 = Any_String        and then Is_String_Type (T1))
        or else (T2 = Any_Character     and then Is_Character_Type (T1))
        or else (T2 = Any_Access        and then Is_Access_Type (T1))
      then
         return True;

      --  The context may be class wide, and a class-wide type is compatible
      --  with any member of the class.

      elsif Is_Class_Wide_Type (T1)
        and then Is_Ancestor (Root_Type (T1), T2)
      then
         return True;

      elsif Is_Class_Wide_Type (T1)
        and then Is_Class_Wide_Type (T2)
        and then Base_Type (Etype (T1)) = Base_Type (Etype (T2))
      then
         return True;

      --  Ada 2005 (AI-345): A class-wide abstract interface type covers a
      --  task_type or protected_type that implements the interface.

      elsif Ada_Version >= Ada_2005
        and then Is_Class_Wide_Type (T1)
        and then Is_Interface (Etype (T1))
        and then Is_Concurrent_Type (T2)
        and then Interface_Present_In_Ancestor
                   (Typ => BT2, Iface => Etype (T1))
      then
         return True;

      --  Ada 2005 (AI-251): A class-wide abstract interface type T1 covers an
      --  object T2 implementing T1.

      elsif Ada_Version >= Ada_2005
        and then Is_Class_Wide_Type (T1)
        and then Is_Interface (Etype (T1))
        and then Is_Tagged_Type (T2)
      then
         if Interface_Present_In_Ancestor (Typ   => T2,
                                           Iface => Etype (T1))
         then
            return True;
         end if;

         declare
            E    : Entity_Id;
            Elmt : Elmt_Id;

         begin
            if Is_Concurrent_Type (BT2) then
               E := Corresponding_Record_Type (BT2);
            else
               E := BT2;
            end if;

            --  Ada 2005 (AI-251): A class-wide abstract interface type T1
            --  covers an object T2 that implements a direct derivation of T1.
            --  Note: test for presence of E is defense against previous error.

            if No (E) then

               --  If expansion is disabled the Corresponding_Record_Type may
               --  not be available yet, so use the interface list in the
               --  declaration directly.

               if ASIS_Mode
                 and then Nkind (Parent (BT2)) = N_Protected_Type_Declaration
                 and then Present (Interface_List (Parent (BT2)))
               then
                  declare
                     Intf : Node_Id := First (Interface_List (Parent (BT2)));
                  begin
                     while Present (Intf) loop
                        if Is_Ancestor (Etype (T1), Entity (Intf)) then
                           return True;
                        else
                           Next (Intf);
                        end if;
                     end loop;
                  end;

                  return False;

               else
                  Check_Error_Detected;
               end if;

            --  Here we have a corresponding record type

            elsif Present (Interfaces (E)) then
               Elmt := First_Elmt (Interfaces (E));
               while Present (Elmt) loop
                  if Is_Ancestor (Etype (T1), Node (Elmt)) then
                     return True;
                  else
                     Next_Elmt (Elmt);
                  end if;
               end loop;
            end if;

            --  We should also check the case in which T1 is an ancestor of
            --  some implemented interface???

            return False;
         end;

      --  In a dispatching call, the formal is of some specific type, and the
      --  actual is of the corresponding class-wide type, including a subtype
      --  of the class-wide type.

      elsif Is_Class_Wide_Type (T2)
        and then
          (Class_Wide_Type (T1) = Class_Wide_Type (T2)
            or else Base_Type (Root_Type (T2)) = BT1)
      then
         return True;

      --  Some contexts require a class of types rather than a specific type.
      --  For example, conditions require any boolean type, fixed point
      --  attributes require some real type, etc. The built-in types Any_XXX
      --  represent these classes.

      elsif     (T1 = Any_Integer  and then Is_Integer_Type     (T2))
        or else (T1 = Any_Boolean  and then Is_Boolean_Type     (T2))
        or else (T1 = Any_Real     and then Is_Real_Type        (T2))
        or else (T1 = Any_Fixed    and then Is_Fixed_Point_Type (T2))
        or else (T1 = Any_Discrete and then Is_Discrete_Type    (T2))
      then
         return True;

      --  An aggregate is compatible with an array or record type

      elsif T2 = Any_Composite and then Is_Aggregate_Type (T1) then
         return True;

      --  If the expected type is an anonymous access, the designated type must
      --  cover that of the expression. Use the base type for this check: even
      --  though access subtypes are rare in sources, they are generated for
      --  actuals in instantiations.

      elsif Ekind (BT1) = E_Anonymous_Access_Type
        and then Is_Access_Type (T2)
        and then Covers (Designated_Type (T1), Designated_Type (T2))
      then
         return True;

      --  Ada 2012 (AI05-0149): Allow an anonymous access type in the context
      --  of a named general access type. An implicit conversion will be
      --  applied. For the resolution, one designated type must cover the
      --  other.

      elsif Ada_Version >= Ada_2012
        and then Ekind (BT1) = E_General_Access_Type
        and then Ekind (BT2) = E_Anonymous_Access_Type
        and then (Covers (Designated_Type (T1), Designated_Type (T2))
                    or else
                  Covers (Designated_Type (T2), Designated_Type (T1)))
      then
         return True;

      --  An Access_To_Subprogram is compatible with itself, or with an
      --  anonymous type created for an attribute reference Access.

      elsif Ekind_In (BT1, E_Access_Subprogram_Type,
                           E_Access_Protected_Subprogram_Type)
        and then Is_Access_Type (T2)
        and then (not Comes_From_Source (T1)
                   or else not Comes_From_Source (T2))
        and then (Is_Overloadable (Designated_Type (T2))
                   or else Ekind (Designated_Type (T2)) = E_Subprogram_Type)
        and then Type_Conformant (Designated_Type (T1), Designated_Type (T2))
        and then Mode_Conformant (Designated_Type (T1), Designated_Type (T2))
      then
         return True;

      --  Ada 2005 (AI-254): An Anonymous_Access_To_Subprogram is compatible
      --  with itself, or with an anonymous type created for an attribute
      --  reference Access.

      elsif Ekind_In (BT1, E_Anonymous_Access_Subprogram_Type,
                           E_Anonymous_Access_Protected_Subprogram_Type)
        and then Is_Access_Type (T2)
        and then (not Comes_From_Source (T1)
                   or else not Comes_From_Source (T2))
        and then (Is_Overloadable (Designated_Type (T2))
                   or else Ekind (Designated_Type (T2)) = E_Subprogram_Type)
        and then Type_Conformant (Designated_Type (T1), Designated_Type (T2))
        and then Mode_Conformant (Designated_Type (T1), Designated_Type (T2))
      then
         return True;

      --  The context can be a remote access type, and the expression the
      --  corresponding source type declared in a categorized package, or
      --  vice versa.

      elsif Is_Record_Type (T1)
        and then (Is_Remote_Call_Interface (T1) or else Is_Remote_Types (T1))
        and then Present (Corresponding_Remote_Type (T1))
      then
         return Covers (Corresponding_Remote_Type (T1), T2);

      --  and conversely.

      elsif Is_Record_Type (T2)
        and then (Is_Remote_Call_Interface (T2) or else Is_Remote_Types (T2))
        and then Present (Corresponding_Remote_Type (T2))
      then
         return Covers (Corresponding_Remote_Type (T2), T1);

      --  Synchronized types are represented at run time by their corresponding
      --  record type. During expansion one is replaced with the other, but
      --  they are compatible views of the same type.

      elsif Is_Record_Type (T1)
        and then Is_Concurrent_Type (T2)
        and then Present (Corresponding_Record_Type (T2))
      then
         return Covers (T1, Corresponding_Record_Type (T2));

      elsif Is_Concurrent_Type (T1)
        and then Present (Corresponding_Record_Type (T1))
        and then Is_Record_Type (T2)
      then
         return Covers (Corresponding_Record_Type (T1), T2);

      --  During analysis, an attribute reference 'Access has a special type
      --  kind: Access_Attribute_Type, to be replaced eventually with the type
      --  imposed by context.

      elsif Ekind (T2) = E_Access_Attribute_Type
        and then Ekind_In (BT1, E_General_Access_Type, E_Access_Type)
        and then Covers (Designated_Type (T1), Designated_Type (T2))
      then
         --  If the target type is a RACW type while the source is an access
         --  attribute type, we are building a RACW that may be exported.

         if Is_Remote_Access_To_Class_Wide_Type (BT1) then
            Set_Has_RACW (Current_Sem_Unit);
         end if;

         return True;

      --  Ditto for allocators, which eventually resolve to the context type

      elsif Ekind (T2) = E_Allocator_Type and then Is_Access_Type (T1) then
         return Covers (Designated_Type (T1), Designated_Type (T2))
           or else
             (From_Limited_With (Designated_Type (T1))
               and then Covers (Designated_Type (T2), Designated_Type (T1)));

      --  A boolean operation on integer literals is compatible with modular
      --  context.

      elsif T2 = Any_Modular and then Is_Modular_Integer_Type (T1) then
         return True;

      --  The actual type may be the result of a previous error

      elsif BT2 = Any_Type then
         return True;

      --  A Raise_Expressions is legal in any expression context

      elsif BT2 = Raise_Type then
         return True;

      --  A packed array type covers its corresponding non-packed type. This is
      --  not legitimate Ada, but allows the omission of a number of otherwise
      --  useless unchecked conversions, and since this can only arise in
      --  (known correct) expanded code, no harm is done.

      elsif Is_Array_Type (T2)
        and then Is_Packed (T2)
        and then T1 = Packed_Array_Impl_Type (T2)
      then
         return True;

      --  Similarly an array type covers its corresponding packed array type

      elsif Is_Array_Type (T1)
        and then Is_Packed (T1)
        and then T2 = Packed_Array_Impl_Type (T1)
      then
         return True;

      --  In instances, or with types exported from instantiations, check
      --  whether a partial and a full view match. Verify that types are
      --  legal, to prevent cascaded errors.

      elsif In_Instance
        and then (Full_View_Covers (T1, T2) or else Full_View_Covers (T2, T1))
      then
         return True;

      elsif Is_Type (T2)
        and then Is_Generic_Actual_Type (T2)
        and then Full_View_Covers (T1, T2)
      then
         return True;

      elsif Is_Type (T1)
        and then Is_Generic_Actual_Type (T1)
        and then Full_View_Covers (T2, T1)
      then
         return True;

      --  In the expansion of inlined bodies, types are compatible if they
      --  are structurally equivalent.

      elsif In_Inlined_Body
        and then (Underlying_Type (T1) = Underlying_Type (T2)
                   or else
                     (Is_Access_Type (T1)
                       and then Is_Access_Type (T2)
                       and then Designated_Type (T1) = Designated_Type (T2))
                   or else
                     (T1 = Any_Access
                       and then Is_Access_Type (Underlying_Type (T2)))
                   or else
                     (T2 = Any_Composite
                       and then Is_Composite_Type (Underlying_Type (T1))))
      then
         return True;

      --  Ada 2005 (AI-50217): Additional branches to make the shadow entity
      --  obtained through a limited_with compatible with its real entity.

      elsif From_Limited_With (T1) then

         --  If the expected type is the nonlimited view of a type, the
         --  expression may have the limited view. If that one in turn is
         --  incomplete, get full view if available.

         return Has_Non_Limited_View (T1)
           and then Covers (Get_Full_View (Non_Limited_View (T1)), T2);

      elsif From_Limited_With (T2) then

         --  If units in the context have Limited_With clauses on each other,
         --  either type might have a limited view. Checks performed elsewhere
         --  verify that the context type is the nonlimited view.

         return Has_Non_Limited_View (T2)
           and then Covers (T1, Get_Full_View (Non_Limited_View (T2)));

      --  Ada 2005 (AI-412): Coverage for regular incomplete subtypes

      elsif Ekind (T1) = E_Incomplete_Subtype then
         return Covers (Full_View (Etype (T1)), T2);

      elsif Ekind (T2) = E_Incomplete_Subtype then
         return Covers (T1, Full_View (Etype (T2)));

      --  Ada 2005 (AI-423): Coverage of formal anonymous access types
      --  and actual anonymous access types in the context of generic
      --  instantiations. We have the following situation:

      --     generic
      --        type Formal is private;
      --        Formal_Obj : access Formal;  --  T1
      --     package G is ...

      --     package P is
      --        type Actual is ...
      --        Actual_Obj : access Actual;  --  T2
      --        package Instance is new G (Formal     => Actual,
      --                                   Formal_Obj => Actual_Obj);

      elsif Ada_Version >= Ada_2005
        and then Ekind (T1) = E_Anonymous_Access_Type
        and then Ekind (T2) = E_Anonymous_Access_Type
        and then Is_Generic_Type (Directly_Designated_Type (T1))
        and then Get_Instance_Of (Directly_Designated_Type (T1)) =
                                               Directly_Designated_Type (T2)
      then
         return True;

      --  Otherwise, types are not compatible

      else
         return False;
      end if;
   end Covers;

   ------------------
   -- Disambiguate --
   ------------------

   function Disambiguate
     (N      : Node_Id;
      I1, I2 : Interp_Index;
      Typ    : Entity_Id) return Interp
   is
      I           : Interp_Index;
      It          : Interp;
      It1, It2    : Interp;
      Nam1, Nam2  : Entity_Id;
      Predef_Subp : Entity_Id;
      User_Subp   : Entity_Id;

      function Inherited_From_Actual (S : Entity_Id) return Boolean;
      --  Determine whether one of the candidates is an operation inherited by
      --  a type that is derived from an actual in an instantiation.

      function In_Same_Declaration_List
        (Typ     : Entity_Id;
         Op_Decl : Entity_Id) return Boolean;
      --  AI05-0020: a spurious ambiguity may arise when equality on anonymous
      --  access types is declared on the partial view of a designated type, so
      --  that the type declaration and equality are not in the same list of
      --  declarations. This AI gives a preference rule for the user-defined
      --  operation. Same rule applies for arithmetic operations on private
      --  types completed with fixed-point types: the predefined operation is
      --  hidden;  this is already handled properly in GNAT.

      function Is_Actual_Subprogram (S : Entity_Id) return Boolean;
      --  Determine whether a subprogram is an actual in an enclosing instance.
      --  An overloading between such a subprogram and one declared outside the
      --  instance is resolved in favor of the first, because it resolved in
      --  the generic. Within the instance the actual is represented by a
      --  constructed subprogram renaming.

      function Matches (Op : Node_Id; Func_Id : Entity_Id) return Boolean;
      --  Determine whether function Func_Id is an exact match for binary or
      --  unary operator Op.

      function Operand_Type return Entity_Id;
      --  Determine type of operand for an equality operation, to apply Ada
      --  2005 rules to equality on anonymous access types.

      function Standard_Operator return Boolean;
      --  Check whether subprogram is predefined operator declared in Standard.
      --  It may given by an operator name, or by an expanded name whose prefix
      --  is Standard.

      function Remove_Conversions return Interp;
      --  Last chance for pathological cases involving comparisons on literals,
      --  and user overloadings of the same operator. Such pathologies have
      --  been removed from the ACVC, but still appear in two DEC tests, with
      --  the following notable quote from Ben Brosgol:
      --
      --  [Note: I disclaim all credit/responsibility/blame for coming up with
      --  this example; Robert Dewar brought it to our attention, since it is
      --  apparently found in the ACVC 1.5. I did not attempt to find the
      --  reason in the Reference Manual that makes the example legal, since I
      --  was too nauseated by it to want to pursue it further.]
      --
      --  Accordingly, this is not a fully recursive solution, but it handles
      --  DEC tests c460vsa, c460vsb. It also handles ai00136a, which pushes
      --  pathology in the other direction with calls whose multiple overloaded
      --  actuals make them truly unresolvable.

      --  The new rules concerning abstract operations create additional need
      --  for special handling of expressions with universal operands, see
      --  comments to Has_Abstract_Interpretation below.

      ---------------------------
      -- Inherited_From_Actual --
      ---------------------------

      function Inherited_From_Actual (S : Entity_Id) return Boolean is
         Par : constant Node_Id := Parent (S);
      begin
         if Nkind (Par) /= N_Full_Type_Declaration
           or else Nkind (Type_Definition (Par)) /= N_Derived_Type_Definition
         then
            return False;
         else
            return Is_Entity_Name (Subtype_Indication (Type_Definition (Par)))
              and then
                Is_Generic_Actual_Type (
                  Entity (Subtype_Indication (Type_Definition (Par))));
         end if;
      end Inherited_From_Actual;

      ------------------------------
      -- In_Same_Declaration_List --
      ------------------------------

      function In_Same_Declaration_List
        (Typ     : Entity_Id;
         Op_Decl : Entity_Id) return Boolean
      is
         Scop : constant Entity_Id := Scope (Typ);

      begin
         return In_Same_List (Parent (Typ), Op_Decl)
           or else
             (Ekind_In (Scop, E_Package, E_Generic_Package)
               and then List_Containing (Op_Decl) =
                              Visible_Declarations (Parent (Scop))
               and then List_Containing (Parent (Typ)) =
                              Private_Declarations (Parent (Scop)));
      end In_Same_Declaration_List;

      --------------------------
      -- Is_Actual_Subprogram --
      --------------------------

      function Is_Actual_Subprogram (S : Entity_Id) return Boolean is
      begin
         return In_Open_Scopes (Scope (S))
           and then Nkind (Unit_Declaration_Node (S)) =
                                         N_Subprogram_Renaming_Declaration

           --  Why the Comes_From_Source test here???

           and then not Comes_From_Source (Unit_Declaration_Node (S))

           and then
             (Is_Generic_Instance (Scope (S))
               or else Is_Wrapper_Package (Scope (S)));
      end Is_Actual_Subprogram;

      -------------
      -- Matches --
      -------------

      function Matches (Op : Node_Id; Func_Id : Entity_Id) return Boolean is
         function Matching_Types
           (Opnd_Typ   : Entity_Id;
            Formal_Typ : Entity_Id) return Boolean;
         --  Determine whether operand type Opnd_Typ and formal parameter type
         --  Formal_Typ are either the same or compatible.

         --------------------
         -- Matching_Types --
         --------------------

         function Matching_Types
           (Opnd_Typ   : Entity_Id;
            Formal_Typ : Entity_Id) return Boolean
         is
         begin
            --  A direct match

            if Opnd_Typ = Formal_Typ then
               return True;

            --  Any integer type matches universal integer

            elsif Opnd_Typ = Universal_Integer
              and then Is_Integer_Type (Formal_Typ)
            then
               return True;

            --  Any floating point type matches universal real

            elsif Opnd_Typ = Universal_Real
              and then Is_Floating_Point_Type (Formal_Typ)
            then
               return True;

            --  The type of the formal parameter maps a generic actual type to
            --  a generic formal type. If the operand type is the type being
            --  mapped in an instance, then this is a match.

            elsif Is_Generic_Actual_Type (Formal_Typ)
              and then Etype (Formal_Typ) = Opnd_Typ
            then
               return True;

            --  ??? There are possibly other cases to consider

            else
               return False;
            end if;
         end Matching_Types;

         --  Local variables

         F1      : constant Entity_Id := First_Formal (Func_Id);
         F1_Typ  : constant Entity_Id := Etype (F1);
         F2      : constant Entity_Id := Next_Formal (F1);
         F2_Typ  : constant Entity_Id := Etype (F2);
         Lop_Typ : constant Entity_Id := Etype (Left_Opnd  (Op));
         Rop_Typ : constant Entity_Id := Etype (Right_Opnd (Op));

      --  Start of processing for Matches

      begin
         if Lop_Typ = F1_Typ then
            return Matching_Types (Rop_Typ, F2_Typ);

         elsif Rop_Typ = F2_Typ then
            return Matching_Types (Lop_Typ, F1_Typ);

         --  Otherwise this is not a good match because each operand-formal
         --  pair is compatible only on base-type basis, which is not specific
         --  enough.

         else
            return False;
         end if;
      end Matches;

      ------------------
      -- Operand_Type --
      ------------------

      function Operand_Type return Entity_Id is
         Opnd : Node_Id;

      begin
         if Nkind (N) = N_Function_Call then
            Opnd := First_Actual (N);
         else
            Opnd := Left_Opnd (N);
         end if;

         return Etype (Opnd);
      end Operand_Type;

      ------------------------
      -- Remove_Conversions --
      ------------------------

      function Remove_Conversions return Interp is
         I    : Interp_Index;
         It   : Interp;
         It1  : Interp;
         F1   : Entity_Id;
         Act1 : Node_Id;
         Act2 : Node_Id;

         function Has_Abstract_Interpretation (N : Node_Id) return Boolean;
         --  If an operation has universal operands the universal operation
         --  is present among its interpretations. If there is an abstract
         --  interpretation for the operator, with a numeric result, this
         --  interpretation was already removed in sem_ch4, but the universal
         --  one is still visible. We must rescan the list of operators and
         --  remove the universal interpretation to resolve the ambiguity.

         ---------------------------------
         -- Has_Abstract_Interpretation --
         ---------------------------------

         function Has_Abstract_Interpretation (N : Node_Id) return Boolean is
            E : Entity_Id;

         begin
            if Nkind (N) not in N_Op
              or else Ada_Version < Ada_2005
              or else not Is_Overloaded (N)
              or else No (Universal_Interpretation (N))
            then
               return False;

            else
               E := Get_Name_Entity_Id (Chars (N));
               while Present (E) loop
                  if Is_Overloadable (E)
                    and then Is_Abstract_Subprogram (E)
                    and then Is_Numeric_Type (Etype (E))
                  then
                     return True;
                  else
                     E := Homonym (E);
                  end if;
               end loop;

               --  Finally, if an operand of the binary operator is itself
               --  an operator, recurse to see whether its own abstract
               --  interpretation is responsible for the spurious ambiguity.

               if Nkind (N) in N_Binary_Op then
                  return Has_Abstract_Interpretation (Left_Opnd (N))
                    or else Has_Abstract_Interpretation (Right_Opnd (N));

               elsif Nkind (N) in N_Unary_Op then
                  return Has_Abstract_Interpretation (Right_Opnd (N));

               else
                  return False;
               end if;
            end if;
         end Has_Abstract_Interpretation;

      --  Start of processing for Remove_Conversions

      begin
         It1 := No_Interp;

         Get_First_Interp (N, I, It);
         while Present (It.Typ) loop
            if not Is_Overloadable (It.Nam) then
               return No_Interp;
            end if;

            F1 := First_Formal (It.Nam);

            if No (F1) then
               return It1;

            else
               if Nkind (N) in N_Subprogram_Call then
                  Act1 := First_Actual (N);

                  if Present (Act1) then
                     Act2 := Next_Actual (Act1);
                  else
                     Act2 := Empty;
                  end if;

               elsif Nkind (N) in N_Unary_Op then
                  Act1 := Right_Opnd (N);
                  Act2 := Empty;

               elsif Nkind (N) in N_Binary_Op then
                  Act1 := Left_Opnd (N);
                  Act2 := Right_Opnd (N);

                  --  Use the type of the second formal, so as to include
                  --  exponentiation, where the exponent may be ambiguous and
                  --  the result non-universal.

                  Next_Formal (F1);

               else
                  return It1;
               end if;

               if Nkind (Act1) in N_Op
                 and then Is_Overloaded (Act1)
                 and then
                   (Nkind (Act1) in N_Unary_Op
                     or else Nkind_In (Left_Opnd (Act1), N_Integer_Literal,
                                                         N_Real_Literal))
                 and then Nkind_In (Right_Opnd (Act1), N_Integer_Literal,
                                                       N_Real_Literal)
                 and then Has_Compatible_Type (Act1, Standard_Boolean)
                 and then Etype (F1) = Standard_Boolean
               then
                  --  If the two candidates are the original ones, the
                  --  ambiguity is real. Otherwise keep the original, further
                  --  calls to Disambiguate will take care of others in the
                  --  list of candidates.

                  if It1 /= No_Interp then
                     if It = Disambiguate.It1
                       or else It = Disambiguate.It2
                     then
                        if It1 = Disambiguate.It1
                          or else It1 = Disambiguate.It2
                        then
                           return No_Interp;
                        else
                           It1 := It;
                        end if;
                     end if;

                  elsif Present (Act2)
                    and then Nkind (Act2) in N_Op
                    and then Is_Overloaded (Act2)
                    and then Nkind_In (Right_Opnd (Act2), N_Integer_Literal,
                                                          N_Real_Literal)
                    and then Has_Compatible_Type (Act2, Standard_Boolean)
                  then
                     --  The preference rule on the first actual is not
                     --  sufficient to disambiguate.

                     goto Next_Interp;

                  else
                     It1 := It;
                  end if;

               elsif Is_Numeric_Type (Etype (F1))
                 and then Has_Abstract_Interpretation (Act1)
               then
                  --  Current interpretation is not the right one because it
                  --  expects a numeric operand. Examine all the other ones.

                  declare
                     I  : Interp_Index;
                     It : Interp;

                  begin
                     Get_First_Interp (N, I, It);
                     while Present (It.Typ) loop
                        if
                          not Is_Numeric_Type (Etype (First_Formal (It.Nam)))
                        then
                           if No (Act2)
                             or else not Has_Abstract_Interpretation (Act2)
                             or else not
                               Is_Numeric_Type
                                 (Etype (Next_Formal (First_Formal (It.Nam))))
                           then
                              return It;
                           end if;
                        end if;

                        Get_Next_Interp (I, It);
                     end loop;

                     return No_Interp;
                  end;
               end if;
            end if;

            <<Next_Interp>>
               Get_Next_Interp (I, It);
         end loop;

         --  After some error, a formal may have Any_Type and yield a spurious
         --  match. To avoid cascaded errors if possible, check for such a
         --  formal in either candidate.

         if Serious_Errors_Detected > 0 then
            declare
               Formal : Entity_Id;

            begin
               Formal := First_Formal (Nam1);
               while Present (Formal) loop
                  if Etype (Formal) = Any_Type then
                     return Disambiguate.It2;
                  end if;

                  Next_Formal (Formal);
               end loop;

               Formal := First_Formal (Nam2);
               while Present (Formal) loop
                  if Etype (Formal) = Any_Type then
                     return Disambiguate.It1;
                  end if;

                  Next_Formal (Formal);
               end loop;
            end;
         end if;

         return It1;
      end Remove_Conversions;

      -----------------------
      -- Standard_Operator --
      -----------------------

      function Standard_Operator return Boolean is
         Nam : Node_Id;

      begin
         if Nkind (N) in N_Op then
            return True;

         elsif Nkind (N) = N_Function_Call then
            Nam := Name (N);

            if Nkind (Nam) /= N_Expanded_Name then
               return True;
            else
               return Entity (Prefix (Nam)) = Standard_Standard;
            end if;
         else
            return False;
         end if;
      end Standard_Operator;

   --  Start of processing for Disambiguate

   begin
      --  Recover the two legal interpretations

      Get_First_Interp (N, I, It);
      while I /= I1 loop
         Get_Next_Interp (I, It);
      end loop;

      It1  := It;
      Nam1 := It.Nam;

      while I /= I2 loop
         Get_Next_Interp (I, It);
      end loop;

      It2  := It;
      Nam2 := It.Nam;

      --  Check whether one of the entities is an Ada 2005/2012 and we are
      --  operating in an earlier mode, in which case we discard the Ada
      --  2005/2012 entity, so that we get proper Ada 95 overload resolution.

      if Ada_Version < Ada_2005 then
         if Is_Ada_2005_Only (Nam1) or else Is_Ada_2012_Only (Nam1) then
            return It2;
         elsif Is_Ada_2005_Only (Nam2) or else Is_Ada_2012_Only (Nam1) then
            return It1;
         end if;
      end if;

      --  Check whether one of the entities is an Ada 2012 entity and we are
      --  operating in Ada 2005 mode, in which case we discard the Ada 2012
      --  entity, so that we get proper Ada 2005 overload resolution.

      if Ada_Version = Ada_2005 then
         if Is_Ada_2012_Only (Nam1) then
            return It2;
         elsif Is_Ada_2012_Only (Nam2) then
            return It1;
         end if;
      end if;

      --  If the context is universal, the predefined operator is preferred.
      --  This includes bounds in numeric type declarations, and expressions
      --  in type conversions. If no interpretation yields a universal type,
      --  then we must check whether the user-defined entity hides the prede-
      --  fined one.

      if Chars (Nam1) in Any_Operator_Name and then Standard_Operator then
         if        Typ = Universal_Integer
           or else Typ = Universal_Real
           or else Typ = Any_Integer
           or else Typ = Any_Discrete
           or else Typ = Any_Real
           or else Typ = Any_Type
         then
            --  Find an interpretation that yields the universal type, or else
            --  a predefined operator that yields a predefined numeric type.

            declare
               Candidate : Interp := No_Interp;

            begin
               Get_First_Interp (N, I, It);
               while Present (It.Typ) loop
                  if (It.Typ = Universal_Integer
                       or else It.Typ = Universal_Real)
                    and then (Typ = Any_Type or else Covers (Typ, It.Typ))
                  then
                     return It;

                  elsif Is_Numeric_Type (It.Typ)
                    and then Scope (It.Typ) = Standard_Standard
                    and then Scope (It.Nam) = Standard_Standard
                    and then Covers (Typ, It.Typ)
                  then
                     Candidate := It;
                  end if;

                  Get_Next_Interp (I, It);
               end loop;

               if Candidate /= No_Interp then
                  return Candidate;
               end if;
            end;

         elsif Chars (Nam1) /= Name_Op_Not
           and then (Typ = Standard_Boolean or else Typ = Any_Boolean)
         then
            --  Equality or comparison operation. Choose predefined operator if
            --  arguments are universal. The node may be an operator, name, or
            --  a function call, so unpack arguments accordingly.

            declare
               Arg1, Arg2 : Node_Id;

            begin
               if Nkind (N) in N_Op then
                  Arg1 := Left_Opnd  (N);
                  Arg2 := Right_Opnd (N);

               elsif Is_Entity_Name (N) then
                  Arg1 := First_Entity (Entity (N));
                  Arg2 := Next_Entity (Arg1);

               else
                  Arg1 := First_Actual (N);
                  Arg2 := Next_Actual (Arg1);
               end if;

               if Present (Arg2)
                 and then Present (Universal_Interpretation (Arg1))
                 and then Universal_Interpretation (Arg2) =
                          Universal_Interpretation (Arg1)
               then
                  Get_First_Interp (N, I, It);
                  while Scope (It.Nam) /= Standard_Standard loop
                     Get_Next_Interp (I, It);
                  end loop;

                  return It;
               end if;
            end;
         end if;
      end if;

      --  If no universal interpretation, check whether user-defined operator
      --  hides predefined one, as well as other special cases. If the node
      --  is a range, then one or both bounds are ambiguous. Each will have
      --  to be disambiguated w.r.t. the context type. The type of the range
      --  itself is imposed by the context, so we can return either legal
      --  interpretation.

      if Ekind (Nam1) = E_Operator then
         Predef_Subp := Nam1;
         User_Subp   := Nam2;

      elsif Ekind (Nam2) = E_Operator then
         Predef_Subp := Nam2;
         User_Subp   := Nam1;

      elsif Nkind (N) = N_Range then
         return It1;

      --  Implement AI05-105: A renaming declaration with an access
      --  definition must resolve to an anonymous access type. This
      --  is a resolution rule and can be used to disambiguate.

      elsif Nkind (Parent (N)) = N_Object_Renaming_Declaration
        and then Present (Access_Definition (Parent (N)))
      then
         if Ekind_In (It1.Typ, E_Anonymous_Access_Type,
                               E_Anonymous_Access_Subprogram_Type)
         then
            if Ekind (It2.Typ) = Ekind (It1.Typ) then

               --  True ambiguity

               return No_Interp;

            else
               return It1;
            end if;

         elsif Ekind_In (It2.Typ, E_Anonymous_Access_Type,
                                  E_Anonymous_Access_Subprogram_Type)
         then
            return It2;

         --  No legal interpretation

         else
            return No_Interp;
         end if;

      --  If two user defined-subprograms are visible, it is a true ambiguity,
      --  unless one of them is an entry and the context is a conditional or
      --  timed entry call, or unless we are within an instance and this is
      --  results from two formals types with the same actual.

      else
         if Nkind (N) = N_Procedure_Call_Statement
           and then Nkind (Parent (N)) = N_Entry_Call_Alternative
           and then N = Entry_Call_Statement (Parent (N))
         then
            if Ekind (Nam2) = E_Entry then
               return It2;
            elsif Ekind (Nam1) = E_Entry then
               return It1;
            else
               return No_Interp;
            end if;

         --  If the ambiguity occurs within an instance, it is due to several
         --  formal types with the same actual. Look for an exact match between
         --  the types of the formals of the overloadable entities, and the
         --  actuals in the call, to recover the unambiguous match in the
         --  original generic.

         --  The ambiguity can also be due to an overloading between a formal
         --  subprogram and a subprogram declared outside the generic. If the
         --  node is overloaded, it did not resolve to the global entity in
         --  the generic, and we choose the formal subprogram.

         --  Finally, the ambiguity can be between an explicit subprogram and
         --  one inherited (with different defaults) from an actual. In this
         --  case the resolution was to the explicit declaration in the
         --  generic, and remains so in the instance.

         --  The same sort of disambiguation needed for calls is also required
         --  for the name given in a subprogram renaming, and that case is
         --  handled here as well. We test Comes_From_Source to exclude this
         --  treatment for implicit renamings created for formal subprograms.

         elsif In_Instance and then not In_Generic_Actual (N) then
            if Nkind (N) in N_Subprogram_Call
              or else
                (Nkind (N) in N_Has_Entity
                  and then
                    Nkind (Parent (N)) = N_Subprogram_Renaming_Declaration
                  and then Comes_From_Source (Parent (N)))
            then
               declare
                  Actual  : Node_Id;
                  Formal  : Entity_Id;
                  Renam   : Entity_Id        := Empty;
                  Is_Act1 : constant Boolean := Is_Actual_Subprogram (Nam1);
                  Is_Act2 : constant Boolean := Is_Actual_Subprogram (Nam2);

               begin
                  if Is_Act1 and then not Is_Act2 then
                     return It1;

                  elsif Is_Act2 and then not Is_Act1 then
                     return It2;

                  elsif Inherited_From_Actual (Nam1)
                    and then Comes_From_Source (Nam2)
                  then
                     return It2;

                  elsif Inherited_From_Actual (Nam2)
                    and then Comes_From_Source (Nam1)
                  then
                     return It1;
                  end if;

                  --  In the case of a renamed subprogram, pick up the entity
                  --  of the renaming declaration so we can traverse its
                  --  formal parameters.

                  if Nkind (N) in N_Has_Entity then
                     Renam := Defining_Unit_Name (Specification (Parent (N)));
                  end if;

                  if Present (Renam) then
                     Actual := First_Formal (Renam);
                  else
                     Actual := First_Actual (N);
                  end if;

                  Formal := First_Formal (Nam1);
                  while Present (Actual) loop
                     if Etype (Actual) /= Etype (Formal) then
                        return It2;
                     end if;

                     if Present (Renam) then
                        Next_Formal (Actual);
                     else
                        Next_Actual (Actual);
                     end if;

                     Next_Formal (Formal);
                  end loop;

                  return It1;
               end;

            elsif Nkind (N) in N_Binary_Op then
               if Matches (N, Nam1) then
                  return It1;
               else
                  return It2;
               end if;

            elsif Nkind (N) in N_Unary_Op then
               if Etype (Right_Opnd (N)) = Etype (First_Formal (Nam1)) then
                  return It1;
               else
                  return It2;
               end if;

            else
               return Remove_Conversions;
            end if;
         else
            return Remove_Conversions;
         end if;
      end if;

      --  An implicit concatenation operator on a string type cannot be
      --  disambiguated from the predefined concatenation. This can only
      --  happen with concatenation of string literals.

      if Chars (User_Subp) = Name_Op_Concat
        and then Ekind (User_Subp) = E_Operator
        and then Is_String_Type (Etype (First_Formal (User_Subp)))
      then
         return No_Interp;

      --  If the user-defined operator is in an open scope, or in the scope
      --  of the resulting type, or given by an expanded name that names its
      --  scope, it hides the predefined operator for the type. Exponentiation
      --  has to be special-cased because the implicit operator does not have
      --  a symmetric signature, and may not be hidden by the explicit one.

      elsif (Nkind (N) = N_Function_Call
              and then Nkind (Name (N)) = N_Expanded_Name
              and then (Chars (Predef_Subp) /= Name_Op_Expon
                         or else Hides_Op (User_Subp, Predef_Subp))
              and then Scope (User_Subp) = Entity (Prefix (Name (N))))
        or else Hides_Op (User_Subp, Predef_Subp)
      then
         if It1.Nam = User_Subp then
            return It1;
         else
            return It2;
         end if;

      --  Otherwise, the predefined operator has precedence, or if the user-
      --  defined operation is directly visible we have a true ambiguity.

      --  If this is a fixed-point multiplication and division in Ada 83 mode,
      --  exclude the universal_fixed operator, which often causes ambiguities
      --  in legacy code.

      --  Ditto in Ada 2012, where an ambiguity may arise for an operation
      --  on a partial view that is completed with a fixed point type. See
      --  AI05-0020 and AI05-0209. The ambiguity is resolved in favor of the
      --  user-defined type and subprogram, so that a client of the package
      --  has the same resolution as the body of the package.

      else
         if (In_Open_Scopes (Scope (User_Subp))
              or else Is_Potentially_Use_Visible (User_Subp))
           and then not In_Instance
         then
            if Is_Fixed_Point_Type (Typ)
              and then Nam_In (Chars (Nam1), Name_Op_Multiply, Name_Op_Divide)
              and then
                (Ada_Version = Ada_83
                  or else (Ada_Version >= Ada_2012
                            and then In_Same_Declaration_List
                                       (First_Subtype (Typ),
                                          Unit_Declaration_Node (User_Subp))))
            then
               if It2.Nam = Predef_Subp then
                  return It1;
               else
                  return It2;
               end if;

            --  Ada 2005, AI-420: preference rule for "=" on Universal_Access
            --  states that the operator defined in Standard is not available
            --  if there is a user-defined equality with the proper signature,
            --  declared in the same declarative list as the type. The node
            --  may be an operator or a function call.

            elsif Nam_In (Chars (Nam1), Name_Op_Eq, Name_Op_Ne)
              and then Ada_Version >= Ada_2005
              and then Etype (User_Subp) = Standard_Boolean
              and then Ekind (Operand_Type) = E_Anonymous_Access_Type
              and then
                In_Same_Declaration_List
                  (Designated_Type (Operand_Type),
                   Unit_Declaration_Node (User_Subp))
            then
               if It2.Nam = Predef_Subp then
                  return It1;
               else
                  return It2;
               end if;

            --  An immediately visible operator hides a use-visible user-
            --  defined operation. This disambiguation cannot take place
            --  earlier because the visibility of the predefined operator
            --  can only be established when operand types are known.

            elsif Ekind (User_Subp) = E_Function
              and then Ekind (Predef_Subp) = E_Operator
              and then Nkind (N) in N_Op
              and then not Is_Overloaded (Right_Opnd (N))
              and then
                Is_Immediately_Visible (Base_Type (Etype (Right_Opnd (N))))
              and then Is_Potentially_Use_Visible (User_Subp)
            then
               if It2.Nam = Predef_Subp then
                  return It1;
               else
                  return It2;
               end if;

            else
               return No_Interp;
            end if;

         elsif It1.Nam = Predef_Subp then
            return It1;

         else
            return It2;
         end if;
      end if;
   end Disambiguate;

   ---------------------
   -- End_Interp_List --
   ---------------------

   procedure End_Interp_List is
   begin
      All_Interp.Table (All_Interp.Last) := No_Interp;
      All_Interp.Increment_Last;
   end End_Interp_List;

   -------------------------
   -- Entity_Matches_Spec --
   -------------------------

   function Entity_Matches_Spec (Old_S, New_S : Entity_Id) return Boolean is
   begin
      --  Simple case: same entity kinds, type conformance is required. A
      --  parameterless function can also rename a literal.

      if Ekind (Old_S) = Ekind (New_S)
        or else (Ekind (New_S) = E_Function
                  and then Ekind (Old_S) = E_Enumeration_Literal)
      then
         return Type_Conformant (New_S, Old_S);

      elsif Ekind (New_S) = E_Function and then Ekind (Old_S) = E_Operator then
         return Operator_Matches_Spec (Old_S, New_S);

      elsif Ekind (New_S) = E_Procedure and then Is_Entry (Old_S) then
         return Type_Conformant (New_S, Old_S);

      else
         return False;
      end if;
   end Entity_Matches_Spec;

   ----------------------
   -- Find_Unique_Type --
   ----------------------

   function Find_Unique_Type (L : Node_Id; R : Node_Id) return Entity_Id is
      T  : constant Entity_Id := Etype (L);
      I  : Interp_Index;
      It : Interp;
      TR : Entity_Id := Any_Type;

   begin
      if Is_Overloaded (R) then
         Get_First_Interp (R, I, It);
         while Present (It.Typ) loop
            if Covers (T, It.Typ) or else Covers (It.Typ, T) then

               --  If several interpretations are possible and L is universal,
               --  apply preference rule.

               if TR /= Any_Type then
                  if (T = Universal_Integer or else T = Universal_Real)
                    and then It.Typ = T
                  then
                     TR := It.Typ;
                  end if;

               else
                  TR := It.Typ;
               end if;
            end if;

            Get_Next_Interp (I, It);
         end loop;

         Set_Etype (R, TR);

      --  In the non-overloaded case, the Etype of R is already set correctly

      else
         null;
      end if;

      --  If one of the operands is Universal_Fixed, the type of the other
      --  operand provides the context.

      if Etype (R) = Universal_Fixed then
         return T;

      elsif T = Universal_Fixed then
         return Etype (R);

      --  Ada 2005 (AI-230): Support the following operators:

      --    function "="  (L, R : universal_access) return Boolean;
      --    function "/=" (L, R : universal_access) return Boolean;

      --  Pool specific access types (E_Access_Type) are not covered by these
      --  operators because of the legality rule of 4.5.2(9.2): "The operands
      --  of the equality operators for universal_access shall be convertible
      --  to one another (see 4.6)". For example, considering the type decla-
      --  ration "type P is access Integer" and an anonymous access to Integer,
      --  P is convertible to "access Integer" by 4.6 (24.11-24.15), but there
      --  is no rule in 4.6 that allows "access Integer" to be converted to P.

      elsif Ada_Version >= Ada_2005
        and then Ekind_In (Etype (L), E_Anonymous_Access_Type,
                                      E_Anonymous_Access_Subprogram_Type)
        and then Is_Access_Type (Etype (R))
        and then Ekind (Etype (R)) /= E_Access_Type
      then
         return Etype (L);

      elsif Ada_Version >= Ada_2005
        and then Ekind_In (Etype (R), E_Anonymous_Access_Type,
                                      E_Anonymous_Access_Subprogram_Type)
        and then Is_Access_Type (Etype (L))
        and then Ekind (Etype (L)) /= E_Access_Type
      then
         return Etype (R);

      --  If one operand is a raise_expression, use type of other operand

      elsif Nkind (L) = N_Raise_Expression then
         return Etype (R);

      else
         return Specific_Type (T, Etype (R));
      end if;
   end Find_Unique_Type;

   -------------------------------------
   -- Function_Interp_Has_Abstract_Op --
   -------------------------------------

   function Function_Interp_Has_Abstract_Op
     (N : Node_Id;
      E : Entity_Id) return Entity_Id
   is
      Abstr_Op  : Entity_Id;
      Act       : Node_Id;
      Act_Parm  : Node_Id;
      Form_Parm : Node_Id;

   begin
      --  Why is check on E needed below ???
      --  In any case this para needs comments ???

      if Is_Overloaded (N) and then Is_Overloadable (E) then
         Act_Parm  := First_Actual (N);
         Form_Parm := First_Formal (E);
         while Present (Act_Parm) and then Present (Form_Parm) loop
            Act := Act_Parm;

            if Nkind (Act) = N_Parameter_Association then
               Act := Explicit_Actual_Parameter (Act);
            end if;

            Abstr_Op := Has_Abstract_Op (Act, Etype (Form_Parm));

            if Present (Abstr_Op) then
               return Abstr_Op;
            end if;

            Next_Actual (Act_Parm);
            Next_Formal (Form_Parm);
         end loop;
      end if;

      return Empty;
   end Function_Interp_Has_Abstract_Op;

   ----------------------
   -- Get_First_Interp --
   ----------------------

   procedure Get_First_Interp
     (N  : Node_Id;
      I  : out Interp_Index;
      It : out Interp)
   is
      Int_Ind : Interp_Index;
      Map_Ptr : Int;
      O_N     : Node_Id;

   begin
      --  If a selected component is overloaded because the selector has
      --  multiple interpretations, the node is a call to a protected
      --  operation or an indirect call. Retrieve the interpretation from
      --  the selector name. The selected component may be overloaded as well
      --  if the prefix is overloaded. That case is unchanged.

      if Nkind (N) = N_Selected_Component
        and then Is_Overloaded (Selector_Name (N))
      then
         O_N := Selector_Name (N);
      else
         O_N := N;
      end if;

      Map_Ptr := Headers (Hash (O_N));
      while Map_Ptr /= No_Entry loop
         if Interp_Map.Table (Map_Ptr).Node = O_N then
            Int_Ind := Interp_Map.Table (Map_Ptr).Index;
            It := All_Interp.Table (Int_Ind);
            I := Int_Ind;
            return;
         else
            Map_Ptr := Interp_Map.Table (Map_Ptr).Next;
         end if;
      end loop;

      --  Procedure should never be called if the node has no interpretations

      raise Program_Error;
   end Get_First_Interp;

   ---------------------
   -- Get_Next_Interp --
   ---------------------

   procedure Get_Next_Interp (I : in out Interp_Index; It : out Interp) is
   begin
      I  := I + 1;
      It := All_Interp.Table (I);
   end Get_Next_Interp;

   -------------------------
   -- Has_Compatible_Type --
   -------------------------

   function Has_Compatible_Type
     (N   : Node_Id;
      Typ : Entity_Id) return Boolean
   is
      I  : Interp_Index;
      It : Interp;

   begin
      if N = Error then
         return False;
      end if;

      if Nkind (N) = N_Subtype_Indication
        or else not Is_Overloaded (N)
      then
         return
           Covers (Typ, Etype (N))

            --  Ada 2005 (AI-345): The context may be a synchronized interface.
            --  If the type is already frozen use the corresponding_record
            --  to check whether it is a proper descendant.

           or else
             (Is_Record_Type (Typ)
               and then Is_Concurrent_Type (Etype (N))
               and then Present (Corresponding_Record_Type (Etype (N)))
               and then Covers (Typ, Corresponding_Record_Type (Etype (N))))

           or else
             (Is_Concurrent_Type (Typ)
               and then Is_Record_Type (Etype (N))
               and then Present (Corresponding_Record_Type (Typ))
               and then Covers (Corresponding_Record_Type (Typ), Etype (N)))

           or else
             (not Is_Tagged_Type (Typ)
               and then Ekind (Typ) /= E_Anonymous_Access_Type
               and then Covers (Etype (N), Typ));

      --  Overloaded case

      else
         Get_First_Interp (N, I, It);
         while Present (It.Typ) loop
            if (Covers (Typ, It.Typ)
                 and then
                   (Scope (It.Nam) /= Standard_Standard
                     or else not Is_Invisible_Operator (N, Base_Type (Typ))))

               --  Ada 2005 (AI-345)

              or else
                (Is_Concurrent_Type (It.Typ)
                  and then Present (Corresponding_Record_Type
                                                             (Etype (It.Typ)))
                  and then Covers (Typ, Corresponding_Record_Type
                                                             (Etype (It.Typ))))

              or else (not Is_Tagged_Type (Typ)
                         and then Ekind (Typ) /= E_Anonymous_Access_Type
                         and then Covers (It.Typ, Typ))
            then
               return True;
            end if;

            Get_Next_Interp (I, It);
         end loop;

         return False;
      end if;
   end Has_Compatible_Type;

   ---------------------
   -- Has_Abstract_Op --
   ---------------------

   function Has_Abstract_Op
     (N   : Node_Id;
      Typ : Entity_Id) return Entity_Id
   is
      I  : Interp_Index;
      It : Interp;

   begin
      if Is_Overloaded (N) then
         Get_First_Interp (N, I, It);
         while Present (It.Nam) loop
            if Present (It.Abstract_Op)
              and then Etype (It.Abstract_Op) = Typ
            then
               return It.Abstract_Op;
            end if;

            Get_Next_Interp (I, It);
         end loop;
      end if;

      return Empty;
   end Has_Abstract_Op;

   ----------
   -- Hash --
   ----------

   function Hash (N : Node_Id) return Int is
   begin
      --  Nodes have a size that is power of two, so to select significant
      --  bits only we remove the low-order bits.

      return ((Int (N) / 2 ** 5) mod Header_Size);
   end Hash;

   --------------
   -- Hides_Op --
   --------------

   function Hides_Op (F : Entity_Id; Op : Entity_Id) return Boolean is
      Btyp : constant Entity_Id := Base_Type (Etype (First_Formal (F)));
   begin
      return Operator_Matches_Spec (Op, F)
        and then (In_Open_Scopes (Scope (F))
                   or else Scope (F) = Scope (Btyp)
                   or else (not In_Open_Scopes (Scope (Btyp))
                             and then not In_Use (Btyp)
                             and then not In_Use (Scope (Btyp))));
   end Hides_Op;

   ------------------------
   -- Init_Interp_Tables --
   ------------------------

   procedure Init_Interp_Tables is
   begin
      All_Interp.Init;
      Interp_Map.Init;
      Headers := (others => No_Entry);
   end Init_Interp_Tables;

   -----------------------------------
   -- Interface_Present_In_Ancestor --
   -----------------------------------

   function Interface_Present_In_Ancestor
     (Typ   : Entity_Id;
      Iface : Entity_Id) return Boolean
   is
      Target_Typ : Entity_Id;
      Iface_Typ  : Entity_Id;

      function Iface_Present_In_Ancestor (Typ : Entity_Id) return Boolean;
      --  Returns True if Typ or some ancestor of Typ implements Iface

      -------------------------------
      -- Iface_Present_In_Ancestor --
      -------------------------------

      function Iface_Present_In_Ancestor (Typ : Entity_Id) return Boolean is
         E    : Entity_Id;
         AI   : Entity_Id;
         Elmt : Elmt_Id;

      begin
         if Typ = Iface_Typ then
            return True;
         end if;

         --  Handle private types

         if Present (Full_View (Typ))
           and then not Is_Concurrent_Type (Full_View (Typ))
         then
            E := Full_View (Typ);
         else
            E := Typ;
         end if;

         loop
            if Present (Interfaces (E))
              and then Present (Interfaces (E))
              and then not Is_Empty_Elmt_List (Interfaces (E))
            then
               Elmt := First_Elmt (Interfaces (E));
               while Present (Elmt) loop
                  AI := Node (Elmt);

                  if AI = Iface_Typ or else Is_Ancestor (Iface_Typ, AI) then
                     return True;
                  end if;

                  Next_Elmt (Elmt);
               end loop;
            end if;

            exit when Etype (E) = E

               --  Handle private types

               or else (Present (Full_View (Etype (E)))
                         and then Full_View (Etype (E)) = E);

            --  Check if the current type is a direct derivation of the
            --  interface

            if Etype (E) = Iface_Typ then
               return True;
            end if;

            --  Climb to the immediate ancestor handling private types

            if Present (Full_View (Etype (E))) then
               E := Full_View (Etype (E));
            else
               E := Etype (E);
            end if;
         end loop;

         return False;
      end Iface_Present_In_Ancestor;

   --  Start of processing for Interface_Present_In_Ancestor

   begin
      --  Iface might be a class-wide subtype, so we have to apply Base_Type

      if Is_Class_Wide_Type (Iface) then
         Iface_Typ := Etype (Base_Type (Iface));
      else
         Iface_Typ := Iface;
      end if;

      --  Handle subtypes

      Iface_Typ := Base_Type (Iface_Typ);

      if Is_Access_Type (Typ) then
         Target_Typ := Etype (Directly_Designated_Type (Typ));
      else
         Target_Typ := Typ;
      end if;

      if Is_Concurrent_Record_Type (Target_Typ) then
         Target_Typ := Corresponding_Concurrent_Type (Target_Typ);
      end if;

      Target_Typ := Base_Type (Target_Typ);

      --  In case of concurrent types we can't use the Corresponding Record_Typ
      --  to look for the interface because it is built by the expander (and
      --  hence it is not always available). For this reason we traverse the
      --  list of interfaces (available in the parent of the concurrent type)

      if Is_Concurrent_Type (Target_Typ) then
         if Present (Interface_List (Parent (Target_Typ))) then
            declare
               AI : Node_Id;

            begin
               AI := First (Interface_List (Parent (Target_Typ)));

               --  The progenitor itself may be a subtype of an interface type.

               while Present (AI) loop
                  if Etype (AI) = Iface_Typ
                    or else Base_Type (Etype (AI)) = Iface_Typ
                  then
                     return True;

                  elsif Present (Interfaces (Etype (AI)))
                    and then Iface_Present_In_Ancestor (Etype (AI))
                  then
                     return True;
                  end if;

                  Next (AI);
               end loop;
            end;
         end if;

         return False;
      end if;

      if Is_Class_Wide_Type (Target_Typ) then
         Target_Typ := Etype (Target_Typ);
      end if;

      if Ekind (Target_Typ) = E_Incomplete_Type then

         --  We must have either a full view or a nonlimited view of the type
         --  to locate the list of ancestors.

         if Present (Full_View (Target_Typ)) then
            Target_Typ := Full_View (Target_Typ);
         else
            pragma Assert (Present (Non_Limited_View (Target_Typ)));
            Target_Typ := Non_Limited_View (Target_Typ);
         end if;

         --  Protect the front end against previously detected errors

         if Ekind (Target_Typ) = E_Incomplete_Type then
            return False;
         end if;
      end if;

      return Iface_Present_In_Ancestor (Target_Typ);
   end Interface_Present_In_Ancestor;

   ---------------------
   -- Intersect_Types --
   ---------------------

   function Intersect_Types (L, R : Node_Id) return Entity_Id is
      Index : Interp_Index;
      It    : Interp;
      Typ   : Entity_Id;

      function Check_Right_Argument (T : Entity_Id) return Entity_Id;
      --  Find interpretation of right arg that has type compatible with T

      --------------------------
      -- Check_Right_Argument --
      --------------------------

      function Check_Right_Argument (T : Entity_Id) return Entity_Id is
         Index : Interp_Index;
         It    : Interp;
         T2    : Entity_Id;

      begin
         if not Is_Overloaded (R) then
            return Specific_Type (T, Etype (R));

         else
            Get_First_Interp (R, Index, It);
            loop
               T2 := Specific_Type (T, It.Typ);

               if T2 /= Any_Type then
                  return T2;
               end if;

               Get_Next_Interp (Index, It);
               exit when No (It.Typ);
            end loop;

            return Any_Type;
         end if;
      end Check_Right_Argument;

   --  Start of processing for Intersect_Types

   begin
      if Etype (L) = Any_Type or else Etype (R) = Any_Type then
         return Any_Type;
      end if;

      if not Is_Overloaded (L) then
         Typ := Check_Right_Argument (Etype (L));

      else
         Typ := Any_Type;
         Get_First_Interp (L, Index, It);
         while Present (It.Typ) loop
            Typ := Check_Right_Argument (It.Typ);
            exit when Typ /= Any_Type;
            Get_Next_Interp (Index, It);
         end loop;

      end if;

      --  If Typ is Any_Type, it means no compatible pair of types was found

      if Typ = Any_Type then
         if Nkind (Parent (L)) in N_Op then
            Error_Msg_N ("incompatible types for operator", Parent (L));

         elsif Nkind (Parent (L)) = N_Range then
            Error_Msg_N ("incompatible types given in constraint", Parent (L));

         --  Ada 2005 (AI-251): Complete the error notification

         elsif Is_Class_Wide_Type (Etype (R))
           and then Is_Interface (Etype (Class_Wide_Type (Etype (R))))
         then
            Error_Msg_NE ("(Ada 2005) does not implement interface }",
                          L, Etype (Class_Wide_Type (Etype (R))));

         --  Specialize message if one operand is a limited view, a priori
         --  unrelated to all other types.

         elsif From_Limited_With (Etype (R)) then
            Error_Msg_NE ("limited view of& not compatible with context",
                           R, Etype (R));

         elsif From_Limited_With (Etype (L)) then
            Error_Msg_NE ("limited view of& not compatible with context",
                           L, Etype (L));
         else
            Error_Msg_N ("incompatible types", Parent (L));
         end if;
      end if;

      return Typ;
   end Intersect_Types;

   -----------------------
   -- In_Generic_Actual --
   -----------------------

   function In_Generic_Actual (Exp : Node_Id) return Boolean is
      Par : constant Node_Id := Parent (Exp);

   begin
      if No (Par) then
         return False;

      elsif Nkind (Par) in N_Declaration then
         if Nkind (Par) = N_Object_Declaration then
            return Present (Corresponding_Generic_Association (Par));
         else
            return False;
         end if;

      elsif Nkind (Par) = N_Object_Renaming_Declaration then
         return Present (Corresponding_Generic_Association (Par));

      elsif Nkind (Par) in N_Statement_Other_Than_Procedure_Call then
         return False;

      else
         return In_Generic_Actual (Parent (Par));
      end if;
   end In_Generic_Actual;

   -----------------
   -- Is_Ancestor --
   -----------------

   function Is_Ancestor
     (T1            : Entity_Id;
      T2            : Entity_Id;
      Use_Full_View : Boolean := False) return Boolean
   is
      BT1 : Entity_Id;
      BT2 : Entity_Id;
      Par : Entity_Id;

   begin
      BT1 := Base_Type (T1);
      BT2 := Base_Type (T2);

      --  Handle underlying view of records with unknown discriminants using
      --  the original entity that motivated the construction of this
      --  underlying record view (see Build_Derived_Private_Type).

      if Is_Underlying_Record_View (BT1) then
         BT1 := Underlying_Record_View (BT1);
      end if;

      if Is_Underlying_Record_View (BT2) then
         BT2 := Underlying_Record_View (BT2);
      end if;

      if BT1 = BT2 then
         return True;

      --  The predicate must look past privacy

      elsif Is_Private_Type (T1)
        and then Present (Full_View (T1))
        and then BT2 = Base_Type (Full_View (T1))
      then
         return True;

      elsif Is_Private_Type (T2)
        and then Present (Full_View (T2))
        and then BT1 = Base_Type (Full_View (T2))
      then
         return True;

      else
         --  Obtain the parent of the base type of T2 (use the full view if
         --  allowed).

         if Use_Full_View
           and then Is_Private_Type (BT2)
           and then Present (Full_View (BT2))
         then
            --  No climbing needed if its full view is the root type

            if Full_View (BT2) = Root_Type (Full_View (BT2)) then
               return False;
            end if;

            Par := Etype (Full_View (BT2));

         else
            Par := Etype (BT2);
         end if;

         loop
            --  If there was a error on the type declaration, do not recurse

            if Error_Posted (Par) then
               return False;

            elsif BT1 = Base_Type (Par)
              or else (Is_Private_Type (T1)
                        and then Present (Full_View (T1))
                        and then Base_Type (Par) = Base_Type (Full_View (T1)))
            then
               return True;

            elsif Is_Private_Type (Par)
              and then Present (Full_View (Par))
              and then Full_View (Par) = BT1
            then
               return True;

            --  Root type found

            elsif Par = Root_Type (Par) then
               return False;

            --  Continue climbing

            else
               --  Use the full-view of private types (if allowed)

               if Use_Full_View
                 and then Is_Private_Type (Par)
                 and then Present (Full_View (Par))
               then
                  Par := Etype (Full_View (Par));
               else
                  Par := Etype (Par);
               end if;
            end if;
         end loop;
      end if;
   end Is_Ancestor;

   ---------------------------
   -- Is_Invisible_Operator --
   ---------------------------

   function Is_Invisible_Operator
     (N : Node_Id;
      T : Entity_Id) return Boolean
   is
      Orig_Node : constant Node_Id := Original_Node (N);

   begin
      if Nkind (N) not in N_Op then
         return False;

      elsif not Comes_From_Source (N) then
         return False;

      elsif No (Universal_Interpretation (Right_Opnd (N))) then
         return False;

      elsif Nkind (N) in N_Binary_Op
        and then No (Universal_Interpretation (Left_Opnd (N)))
      then
         return False;

      else
         return Is_Numeric_Type (T)
           and then not In_Open_Scopes (Scope (T))
           and then not Is_Potentially_Use_Visible (T)
           and then not In_Use (T)
           and then not In_Use (Scope (T))
           and then
            (Nkind (Orig_Node) /= N_Function_Call
              or else Nkind (Name (Orig_Node)) /= N_Expanded_Name
              or else Entity (Prefix (Name (Orig_Node))) /= Scope (T))
           and then not In_Instance;
      end if;
   end Is_Invisible_Operator;

   --------------------
   --  Is_Progenitor --
   --------------------

   function Is_Progenitor
     (Iface : Entity_Id;
      Typ   : Entity_Id) return Boolean
   is
   begin
      return Implements_Interface (Typ, Iface, Exclude_Parents => True);
   end Is_Progenitor;

   -------------------
   -- Is_Subtype_Of --
   -------------------

   function Is_Subtype_Of (T1 : Entity_Id; T2 : Entity_Id) return Boolean is
      S : Entity_Id;

   begin
      S := Ancestor_Subtype (T1);
      while Present (S) loop
         if S = T2 then
            return True;
         else
            S := Ancestor_Subtype (S);
         end if;
      end loop;

      return False;
   end Is_Subtype_Of;

   ------------------
   -- List_Interps --
   ------------------

   procedure List_Interps (Nam : Node_Id; Err : Node_Id) is
      Index : Interp_Index;
      It    : Interp;

   begin
      Get_First_Interp (Nam, Index, It);
      while Present (It.Nam) loop
         if Scope (It.Nam) = Standard_Standard
           and then Scope (It.Typ) /= Standard_Standard
         then
            Error_Msg_Sloc := Sloc (Parent (It.Typ));
            Error_Msg_NE ("\\& (inherited) declared#!", Err, It.Nam);

         else
            Error_Msg_Sloc := Sloc (It.Nam);
            Error_Msg_NE ("\\& declared#!", Err, It.Nam);
         end if;

         Get_Next_Interp (Index, It);
      end loop;
   end List_Interps;

   -----------------
   -- New_Interps --
   -----------------

   procedure New_Interps (N : Node_Id) is
      Map_Ptr : Int;

   begin
      All_Interp.Append (No_Interp);

      Map_Ptr := Headers (Hash (N));

      if Map_Ptr = No_Entry then

         --  Place new node at end of table

         Interp_Map.Increment_Last;
         Headers (Hash (N)) := Interp_Map.Last;

      else
         --   Place node at end of chain, or locate its previous entry

         loop
            if Interp_Map.Table (Map_Ptr).Node = N then

               --  Node is already in the table, and is being rewritten.
               --  Start a new interp section, retain hash link.

               Interp_Map.Table (Map_Ptr).Node  := N;
               Interp_Map.Table (Map_Ptr).Index := All_Interp.Last;
               Set_Is_Overloaded (N, True);
               return;

            else
               exit when Interp_Map.Table (Map_Ptr).Next = No_Entry;
               Map_Ptr := Interp_Map.Table (Map_Ptr).Next;
            end if;
         end loop;

         --  Chain the new node

         Interp_Map.Increment_Last;
         Interp_Map.Table (Map_Ptr).Next := Interp_Map.Last;
      end if;

      Interp_Map.Table (Interp_Map.Last) := (N, All_Interp.Last, No_Entry);
      Set_Is_Overloaded (N, True);
   end New_Interps;

   ---------------------------
   -- Operator_Matches_Spec --
   ---------------------------

   function Operator_Matches_Spec (Op, New_S : Entity_Id) return Boolean is
      New_First_F : constant Entity_Id := First_Formal (New_S);
      Op_Name     : constant Name_Id   := Chars (Op);
      T           : constant Entity_Id := Etype (New_S);
      New_F       : Entity_Id;
      Num         : Nat;
      Old_F       : Entity_Id;
      T1          : Entity_Id;
      T2          : Entity_Id;

   begin
      --  To verify that a predefined operator matches a given signature, do a
      --  case analysis of the operator classes. Function can have one or two
      --  formals and must have the proper result type.

      New_F := New_First_F;
      Old_F := First_Formal (Op);
      Num := 0;
      while Present (New_F) and then Present (Old_F) loop
         Num := Num + 1;
         Next_Formal (New_F);
         Next_Formal (Old_F);
      end loop;

      --  Definite mismatch if different number of parameters

      if Present (Old_F) or else Present (New_F) then
         return False;

      --  Unary operators

      elsif Num = 1 then
         T1 := Etype (New_First_F);

         if Nam_In (Op_Name, Name_Op_Subtract, Name_Op_Add, Name_Op_Abs) then
            return Base_Type (T1) = Base_Type (T)
              and then Is_Numeric_Type (T);

         elsif Op_Name = Name_Op_Not then
            return Base_Type (T1) = Base_Type (T)
              and then Valid_Boolean_Arg (Base_Type (T));

         else
            return False;
         end if;

      --  Binary operators

      else
         T1 := Etype (New_First_F);
         T2 := Etype (Next_Formal (New_First_F));

         if Nam_In (Op_Name, Name_Op_And, Name_Op_Or, Name_Op_Xor) then
            return Base_Type (T1) = Base_Type (T2)
              and then Base_Type (T1) = Base_Type (T)
              and then Valid_Boolean_Arg (Base_Type (T));

         elsif Nam_In (Op_Name, Name_Op_Eq, Name_Op_Ne) then
            return Base_Type (T1) = Base_Type (T2)
              and then not Is_Limited_Type (T1)
              and then Is_Boolean_Type (T);

         elsif Nam_In (Op_Name, Name_Op_Lt, Name_Op_Le,
                                Name_Op_Gt, Name_Op_Ge)
         then
            return Base_Type (T1) = Base_Type (T2)
              and then Valid_Comparison_Arg (T1)
              and then Is_Boolean_Type (T);

         elsif Nam_In (Op_Name, Name_Op_Add, Name_Op_Subtract) then
            return Base_Type (T1) = Base_Type (T2)
              and then Base_Type (T1) = Base_Type (T)
              and then Is_Numeric_Type (T);

         --  For division and multiplication, a user-defined function does not
         --  match the predefined universal_fixed operation, except in Ada 83.

         elsif Op_Name = Name_Op_Divide then
            return (Base_Type (T1) = Base_Type (T2)
              and then Base_Type (T1) = Base_Type (T)
              and then Is_Numeric_Type (T)
              and then (not Is_Fixed_Point_Type (T)
                         or else Ada_Version = Ada_83))

            --  Mixed_Mode operations on fixed-point types

              or else (Base_Type (T1) = Base_Type (T)
                        and then Base_Type (T2) = Base_Type (Standard_Integer)
                        and then Is_Fixed_Point_Type (T))

            --  A user defined operator can also match (and hide) a mixed
            --  operation on universal literals.

              or else (Is_Integer_Type (T2)
                        and then Is_Floating_Point_Type (T1)
                        and then Base_Type (T1) = Base_Type (T));

         elsif Op_Name = Name_Op_Multiply then
            return (Base_Type (T1) = Base_Type (T2)
              and then Base_Type (T1) = Base_Type (T)
              and then Is_Numeric_Type (T)
              and then (not Is_Fixed_Point_Type (T)
                         or else Ada_Version = Ada_83))

            --  Mixed_Mode operations on fixed-point types

              or else (Base_Type (T1) = Base_Type (T)
                        and then Base_Type (T2) = Base_Type (Standard_Integer)
                        and then Is_Fixed_Point_Type (T))

              or else (Base_Type (T2) = Base_Type (T)
                        and then Base_Type (T1) = Base_Type (Standard_Integer)
                        and then Is_Fixed_Point_Type (T))

              or else (Is_Integer_Type (T2)
                        and then Is_Floating_Point_Type (T1)
                        and then Base_Type (T1) = Base_Type (T))

              or else (Is_Integer_Type (T1)
                        and then Is_Floating_Point_Type (T2)
                        and then Base_Type (T2) = Base_Type (T));

         elsif Nam_In (Op_Name, Name_Op_Mod, Name_Op_Rem) then
            return Base_Type (T1) = Base_Type (T2)
              and then Base_Type (T1) = Base_Type (T)
              and then Is_Integer_Type (T);

         elsif Op_Name = Name_Op_Expon then
            return Base_Type (T1) = Base_Type (T)
              and then Is_Numeric_Type (T)
              and then Base_Type (T2) = Base_Type (Standard_Integer);

         elsif Op_Name = Name_Op_Concat then
            return Is_Array_Type (T)
              and then (Base_Type (T) = Base_Type (Etype (Op)))
              and then (Base_Type (T1) = Base_Type (T)
                          or else
                        Base_Type (T1) = Base_Type (Component_Type (T)))
              and then (Base_Type (T2) = Base_Type (T)
                          or else
                        Base_Type (T2) = Base_Type (Component_Type (T)));

         else
            return False;
         end if;
      end if;
   end Operator_Matches_Spec;

   -------------------
   -- Remove_Interp --
   -------------------

   procedure Remove_Interp (I : in out Interp_Index) is
      II : Interp_Index;

   begin
      --  Find end of interp list and copy downward to erase the discarded one

      II := I + 1;
      while Present (All_Interp.Table (II).Typ) loop
         II := II + 1;
      end loop;

      for J in I + 1 .. II loop
         All_Interp.Table (J - 1) := All_Interp.Table (J);
      end loop;

      --  Back up interp index to insure that iterator will pick up next
      --  available interpretation.

      I := I - 1;
   end Remove_Interp;

   ------------------
   -- Save_Interps --
   ------------------

   procedure Save_Interps (Old_N : Node_Id; New_N : Node_Id) is
      Map_Ptr : Int;
      O_N     : Node_Id := Old_N;

   begin
      if Is_Overloaded (Old_N) then
         Set_Is_Overloaded (New_N);

         if Nkind (Old_N) = N_Selected_Component
           and then Is_Overloaded (Selector_Name (Old_N))
         then
            O_N := Selector_Name (Old_N);
         end if;

         Map_Ptr := Headers (Hash (O_N));

         while Interp_Map.Table (Map_Ptr).Node /= O_N loop
            Map_Ptr := Interp_Map.Table (Map_Ptr).Next;
            pragma Assert (Map_Ptr /= No_Entry);
         end loop;

         New_Interps (New_N);
         Interp_Map.Table (Interp_Map.Last).Index :=
           Interp_Map.Table (Map_Ptr).Index;
      end if;
   end Save_Interps;

   -------------------
   -- Specific_Type --
   -------------------

   function Specific_Type (Typ_1, Typ_2 : Entity_Id) return Entity_Id is
      T1 : constant Entity_Id := Available_View (Typ_1);
      T2 : constant Entity_Id := Available_View (Typ_2);
      B1 : constant Entity_Id := Base_Type (T1);
      B2 : constant Entity_Id := Base_Type (T2);

      function Is_Remote_Access (T : Entity_Id) return Boolean;
      --  Check whether T is the equivalent type of a remote access type.
      --  If distribution is enabled, T is a legal context for Null.

      ----------------------
      -- Is_Remote_Access --
      ----------------------

      function Is_Remote_Access (T : Entity_Id) return Boolean is
      begin
         return Is_Record_Type (T)
           and then (Is_Remote_Call_Interface (T)
                      or else Is_Remote_Types (T))
           and then Present (Corresponding_Remote_Type (T))
           and then Is_Access_Type (Corresponding_Remote_Type (T));
      end Is_Remote_Access;

   --  Start of processing for Specific_Type

   begin
      if T1 = Any_Type or else T2 = Any_Type then
         return Any_Type;
      end if;

      if B1 = B2 then
         return B1;

      elsif     (T1 = Universal_Integer and then Is_Integer_Type (T2))
        or else (T1 = Universal_Real    and then Is_Real_Type (T2))
        or else (T1 = Universal_Fixed   and then Is_Fixed_Point_Type (T2))
        or else (T1 = Any_Fixed         and then Is_Fixed_Point_Type (T2))
      then
         return B2;

      elsif     (T2 = Universal_Integer and then Is_Integer_Type (T1))
        or else (T2 = Universal_Real    and then Is_Real_Type (T1))
        or else (T2 = Universal_Fixed   and then Is_Fixed_Point_Type (T1))
        or else (T2 = Any_Fixed         and then Is_Fixed_Point_Type (T1))
      then
         return B1;

      elsif T2 = Any_String and then Is_String_Type (T1) then
         return B1;

      elsif T1 = Any_String and then Is_String_Type (T2) then
         return B2;

      elsif T2 = Any_Character and then Is_Character_Type (T1) then
         return B1;

      elsif T1 = Any_Character and then Is_Character_Type (T2) then
         return B2;

      elsif T1 = Any_Access
        and then (Is_Access_Type (T2) or else Is_Remote_Access (T2))
      then
         return T2;

      elsif T2 = Any_Access
        and then (Is_Access_Type (T1) or else Is_Remote_Access (T1))
      then
         return T1;

      --  In an instance, the specific type may have a private view. Use full
      --  view to check legality.

      elsif T2 = Any_Access
        and then Is_Private_Type (T1)
        and then Present (Full_View (T1))
        and then Is_Access_Type (Full_View (T1))
        and then In_Instance
      then
         return T1;

      elsif T2 = Any_Composite and then Is_Aggregate_Type (T1) then
         return T1;

      elsif T1 = Any_Composite and then Is_Aggregate_Type (T2) then
         return T2;

      elsif T1 = Any_Modular and then Is_Modular_Integer_Type (T2) then
         return T2;

      elsif T2 = Any_Modular and then Is_Modular_Integer_Type (T1) then
         return T1;

      --  ----------------------------------------------------------
      --  Special cases for equality operators (all other predefined
      --  operators can never apply to tagged types)
      --  ----------------------------------------------------------

      --  Ada 2005 (AI-251): T1 and T2 are class-wide types, and T2 is an
      --  interface

      elsif Is_Class_Wide_Type (T1)
        and then Is_Class_Wide_Type (T2)
        and then Is_Interface (Etype (T2))
      then
         return T1;

      --  Ada 2005 (AI-251): T1 is a concrete type that implements the
      --  class-wide interface T2

      elsif Is_Class_Wide_Type (T2)
        and then Is_Interface (Etype (T2))
        and then Interface_Present_In_Ancestor (Typ   => T1,
                                                Iface => Etype (T2))
      then
         return T1;

      elsif Is_Class_Wide_Type (T1)
        and then Is_Ancestor (Root_Type (T1), T2)
      then
         return T1;

      elsif Is_Class_Wide_Type (T2)
        and then Is_Ancestor (Root_Type (T2), T1)
      then
         return T2;

      elsif Ekind_In (B1, E_Access_Subprogram_Type,
                          E_Access_Protected_Subprogram_Type)
        and then Ekind (Designated_Type (B1)) /= E_Subprogram_Type
        and then Is_Access_Type (T2)
      then
         return T2;

      elsif Ekind_In (B2, E_Access_Subprogram_Type,
                          E_Access_Protected_Subprogram_Type)
        and then Ekind (Designated_Type (B2)) /= E_Subprogram_Type
        and then Is_Access_Type (T1)
      then
         return T1;

      elsif Ekind_In (T1, E_Allocator_Type,
                          E_Access_Attribute_Type,
                          E_Anonymous_Access_Type)
        and then Is_Access_Type (T2)
      then
         return T2;

      elsif Ekind_In (T2, E_Allocator_Type,
                          E_Access_Attribute_Type,
                          E_Anonymous_Access_Type)
        and then Is_Access_Type (T1)
      then
         return T1;

      --  If none of the above cases applies, types are not compatible

      else
         return Any_Type;
      end if;
   end Specific_Type;

   ---------------------
   -- Set_Abstract_Op --
   ---------------------

   procedure Set_Abstract_Op (I : Interp_Index; V : Entity_Id) is
   begin
      All_Interp.Table (I).Abstract_Op := V;
   end Set_Abstract_Op;

   -----------------------
   -- Valid_Boolean_Arg --
   -----------------------

   --  In addition to booleans and arrays of booleans, we must include
   --  aggregates as valid boolean arguments, because in the first pass of
   --  resolution their components are not examined. If it turns out not to be
   --  an aggregate of booleans, this will be diagnosed in Resolve.
   --  Any_Composite must be checked for prior to the array type checks because
   --  Any_Composite does not have any associated indexes.

   function Valid_Boolean_Arg (T : Entity_Id) return Boolean is
   begin
      if Is_Boolean_Type (T)
        or else Is_Modular_Integer_Type (T)
        or else T = Universal_Integer
        or else T = Any_Composite
      then
         return True;

      elsif Is_Array_Type (T)
        and then T /= Any_String
        and then Number_Dimensions (T) = 1
        and then Is_Boolean_Type (Component_Type (T))
        and then
         ((not Is_Private_Composite (T) and then not Is_Limited_Composite (T))
           or else In_Instance
           or else Available_Full_View_Of_Component (T))
      then
         return True;

      else
         return False;
      end if;
   end Valid_Boolean_Arg;

   --------------------------
   -- Valid_Comparison_Arg --
   --------------------------

   function Valid_Comparison_Arg (T : Entity_Id) return Boolean is
   begin

      if T = Any_Composite then
         return False;

      elsif Is_Discrete_Type (T)
        or else Is_Real_Type (T)
      then
         return True;

      elsif Is_Array_Type (T)
          and then Number_Dimensions (T) = 1
          and then Is_Discrete_Type (Component_Type (T))
          and then (not Is_Private_Composite (T) or else In_Instance)
          and then (not Is_Limited_Composite (T) or else In_Instance)
      then
         return True;

      elsif Is_Array_Type (T)
        and then Number_Dimensions (T) = 1
        and then Is_Discrete_Type (Component_Type (T))
        and then Available_Full_View_Of_Component (T)
      then
         return True;

      elsif Is_String_Type (T) then
         return True;
      else
         return False;
      end if;
   end Valid_Comparison_Arg;

   ------------------
   -- Write_Interp --
   ------------------

   procedure Write_Interp (It : Interp) is
   begin
      Write_Str ("Nam: ");
      Print_Tree_Node (It.Nam);
      Write_Str ("Typ: ");
      Print_Tree_Node (It.Typ);
      Write_Str ("Abstract_Op: ");
      Print_Tree_Node (It.Abstract_Op);
   end Write_Interp;

   ----------------------
   -- Write_Interp_Ref --
   ----------------------

   procedure Write_Interp_Ref (Map_Ptr : Int) is
   begin
      Write_Str (" Node:  ");
      Write_Int (Int (Interp_Map.Table (Map_Ptr).Node));
      Write_Str (" Index: ");
      Write_Int (Int (Interp_Map.Table (Map_Ptr).Index));
      Write_Str (" Next:  ");
      Write_Int (Interp_Map.Table (Map_Ptr).Next);
      Write_Eol;
   end Write_Interp_Ref;

   ---------------------
   -- Write_Overloads --
   ---------------------

   procedure Write_Overloads (N : Node_Id) is
      I   : Interp_Index;
      It  : Interp;
      Nam : Entity_Id;

   begin
      Write_Str ("Overloads: ");
      Print_Node_Briefly (N);

      if not Is_Overloaded (N) then
         Write_Line ("Non-overloaded entity ");
         Write_Entity_Info (Entity (N), " ");

      elsif Nkind (N) not in N_Has_Entity then
         Get_First_Interp (N, I, It);
         while Present (It.Nam) loop
            Write_Int (Int (It.Typ));
            Write_Str ("   ");
            Write_Name (Chars (It.Typ));
            Write_Eol;
            Get_Next_Interp (I, It);
         end loop;

      else
         Get_First_Interp (N, I, It);
         Write_Line ("Overloaded entity ");
         Write_Line ("      Name           Type           Abstract Op");
         Write_Line ("===============================================");
         Nam := It.Nam;

         while Present (Nam) loop
            Write_Int (Int (Nam));
            Write_Str ("   ");
            Write_Name (Chars (Nam));
            Write_Str ("   ");
            Write_Int (Int (It.Typ));
            Write_Str ("   ");
            Write_Name (Chars (It.Typ));

            if Present (It.Abstract_Op) then
               Write_Str ("   ");
               Write_Int (Int (It.Abstract_Op));
               Write_Str ("   ");
               Write_Name (Chars (It.Abstract_Op));
            end if;

            Write_Eol;
            Get_Next_Interp (I, It);
            Nam := It.Nam;
         end loop;
      end if;
   end Write_Overloads;

end Sem_Type;
