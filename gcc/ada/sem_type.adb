------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ T Y P E                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

with Aspects;        use Aspects;
with Atree;          use Atree;
with Alloc;
with Debug;          use Debug;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Elists;         use Elists;
with Nlists;         use Nlists;
with Errout;         use Errout;
with Lib;            use Lib;
with Namet;          use Namet;
with Opt;            use Opt;
with Output;         use Output;
with Sem;            use Sem;
with Sem_Aux;        use Sem_Aux;
with Sem_Ch6;        use Sem_Ch6;
with Sem_Ch8;        use Sem_Ch8;
with Sem_Ch12;       use Sem_Ch12;
with Sem_Disp;       use Sem_Disp;
with Sem_Dist;       use Sem_Dist;
with Sem_Util;       use Sem_Util;
with Stand;          use Stand;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Snames;         use Snames;
with Table;
with Treepr;         use Treepr;
with Uintp;          use Uintp;

with GNAT.HTable;    use GNAT.HTable;

package body Sem_Type is

   ---------------------
   -- Data Structures --
   ---------------------

   --  The following data structures establish a mapping between nodes and
   --  their interpretations. An overloaded node has an entry in Interp_Map,
   --  which in turn contains a pointer into the All_Interp array. The
   --  interpretations of a given node are contiguous in All_Interp. Each set
   --  of interpretations is terminated with the marker No_Interp.

   --     Interp_Map           All_Interp

   --      +-----+             +--------+
   --      |     |         --->|interp1 |
   --      |_____|         |   |interp2 |
   --      |index|---------|   |nointerp|
   --      |-----|             |        |
   --      |     |             |        |
   --      +-----+             +--------+

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

   Header_Max : constant := 3079;
   --  The number of hash buckets; an arbitrary prime number

   subtype Header_Num is Integer range 0 .. Header_Max - 1;

   function Hash (N : Node_Id) return Header_Num;
   --  A trivial hashing function for nodes, used to insert an overloaded
   --  node into the Interp_Map table.

   package Interp_Map is new Simple_HTable
     (Header_Num => Header_Num,
      Element    => Interp_Index,
      No_Element => -1,
      Key        => Node_Id,
      Hash       => Hash,
      Equal      => "=");

   Last_Overloaded : Node_Id := Empty;
   --  Overloaded node after initializing a new collection of intepretation

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

   --------------------
   -- Add_One_Interp --
   --------------------

   procedure Add_One_Interp
     (N        : Node_Id;
      E        : Entity_Id;
      T        : Entity_Id;
      Opnd_Typ : Entity_Id := Empty)
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
            elsif Nkind (N) = N_Function_Call
              and then Ekind (Name) = E_Function
            then
               Abstr_Op := Function_Interp_Has_Abstract_Op (N, Name);
            end if;
         end if;

         Get_First_Interp (N, I, It);
         while Present (It.Nam) loop

            --  Avoid making duplicate entries in overloads

            if Name = It.Nam
              and then Base_Type (It.Typ) = Base_Type (T)
            then
               return;

            --  A user-defined subprogram hides another declared at an outer
            --  level, or one that is use-visible. So return if previous
            --  definition hides new one (which is either in an outer
            --  scope, or use-visible). Note that for functions use-visible
            --  is the same as potentially use-visible. If new one hides
            --  previous one, replace entry in table of interpretations.
            --  If this is a universal operation, retain the operator in case
            --  preference rule applies.

            elsif ((Ekind (Name) in E_Function | E_Procedure
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

            --  Otherwise keep going

            else
               Get_Next_Interp (I, It);
            end if;
         end loop;

         All_Interp.Table (All_Interp.Last) := (Name, Typ, Opnd_Typ, Abstr_Op);
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
            if Present (Universal_Interpretation (Left_Opnd (N)))
              and then Present (Universal_Interpretation (Right_Opnd (N)))
            then
               return True;
            elsif Nkind (N) in N_Op_Eq | N_Op_Ne
              and then
                (Is_Anonymous_Access_Type (Etype (Left_Opnd (N)))
                  or else Is_Anonymous_Access_Type (Etype (Right_Opnd (N))))
            then
               return True;
            else
               return False;
            end if;

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
      --  If the interpretation is a predefined operator, verify that it is
      --  visible, or that the entity has already been resolved (case of an
      --  instantiation node that refers to a predefined operation, or an
      --  internally generated operator node, or an operator given as an
      --  expanded name). If the operator is a comparison or equality, then
      --  it is the type of the operand that is relevant here.

      if Ekind (E) = E_Operator then
         if Present (Opnd_Typ) then
            Vis_Type := Opnd_Typ;
         else
            Vis_Type := Base_Type (T);
         end if;

         if Nkind (N) = N_Expanded_Name
           or else (Nkind (N) in N_Op and then E = Entity (N))
           or else Is_Visible_Operator (N, Vis_Type)
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

         --  Otherwise this is the first interpretation, N has type Any_Type
         --  and we must place the new type on the node.

         else
            Set_Etype (N, T);
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

      elsif No (Last_Overloaded)
        or else
          (Last_Overloaded /= N
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
                          (H, Etype (H), Empty, Empty);
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
      --  create an accidental ambiguity, and the check on compatibility of
      --  generic actual types must use this enclosing actual.

      ----------------------
      -- Full_View_Covers --
      ----------------------

      function Full_View_Covers (Typ1, Typ2 : Entity_Id) return Boolean is
      begin
         if Present (Full_View (Typ1))
           and then Covers (Full_View (Typ1), Typ2)
         then
            return True;

         elsif Present (Underlying_Full_View (Typ1))
           and then Covers (Underlying_Full_View (Typ1), Typ2)
         then
            return True;

         else
            return False;
         end if;
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
      --  If either operand is missing, then this is an error, but ignore it
      --  and pretend we have a cover if errors already detected since this may
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

      if T1 = Standard_Void_Type or else T2 = Standard_Void_Type then
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

      --  This test may seem to be redundant with the above one, but it catches
      --  peculiar cases where a private type declared in a package is used in
      --  a generic construct declared in another package, and the body of the
      --  former package contains an instantiation of the generic construct on
      --  an object whose type is a subtype of the private type; in this case,
      --  the subtype is not private but the type is private in the instance.

      elsif Is_Subtype_Of (T1 => T2, T2 => T1) then
         return True;

      --  Literals are compatible with types in a given "class"

      elsif     (T2 = Universal_Integer and then Is_Integer_Type (T1))
        or else (T2 = Universal_Real    and then Is_Real_Type (T1))
        or else (T2 = Universal_Fixed   and then Is_Fixed_Point_Type (T1))
        or else (T2 = Universal_Access  and then Is_Access_Type (T1))
        or else (T2 = Any_Fixed         and then Is_Fixed_Point_Type (T1))
        or else (T2 = Any_Character     and then Is_Character_Type (T1))
        or else (T2 = Any_String        and then Is_String_Type (T1))
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
        and then Is_Concurrent_Type (T2)
        and then Is_Class_Wide_Type (T1)
        and then Is_Interface (Etype (T1))
        and then Interface_Present_In_Ancestor
                   (Typ => BT2, Iface => Etype (T1))
      then
         return True;

      --  Ada 2005 (AI-251): A class-wide abstract interface type T1 covers an
      --  object T2 implementing T1.

      elsif Ada_Version >= Ada_2005
        and then Is_Tagged_Type (T2)
        and then Is_Class_Wide_Type (T1)
        and then Is_Interface (Etype (T1))
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
               Check_Error_Detected;

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

      --  In Ada_2022, an aggregate is compatible with the type that
      --  as the corresponding aspect.

      elsif Ada_Version >= Ada_2022
        and then T2 = Any_Composite
        and then Has_Aspect (T1, Aspect_Aggregate)
      then
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
      --  applied. For the resolution, the designated types must match if
      --  untagged; further, if the designated type is tagged, the designated
      --  type of the anonymous access type shall be covered by the designated
      --  type of the named access type.

      elsif Ada_Version >= Ada_2012
        and then Ekind (BT1) = E_General_Access_Type
        and then Ekind (BT2) = E_Anonymous_Access_Type
        and then Covers (Designated_Type (T1), Designated_Type (T2))
        and then Is_Class_Wide_Type (Designated_Type (T1)) >=
                 Is_Class_Wide_Type (Designated_Type (T2))
      then
         return True;

      --  An Access_To_Subprogram is compatible with itself, or with an
      --  anonymous type created for an attribute reference Access.

      elsif Ekind (BT1) in E_Access_Subprogram_Type
                         | E_Access_Protected_Subprogram_Type
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

      elsif Ekind (BT1) in E_Anonymous_Access_Subprogram_Type
                         | E_Anonymous_Access_Protected_Subprogram_Type
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
        and then Ekind (BT1) in E_General_Access_Type | E_Access_Type
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

      elsif Is_Packed_Array (T2)
        and then T1 = Packed_Array_Impl_Type (T2)
      then
         return True;

      --  Similarly an array type covers its corresponding packed array type

      elsif Is_Packed_Array (T1)
        and then T2 = Packed_Array_Impl_Type (T1)
      then
         return True;

      --  With types exported from instantiations, check whether a partial and
      --  a full view match. Verify that types are legal, to prevent cascaded
      --  errors.

      elsif Is_Private_Type (T1)
        and then Is_Type (T2)
        and then Is_Generic_Actual_Type (T2)
        and then Full_View_Covers (T1, T2)
      then
         return True;

      elsif Is_Private_Type (T2)
        and then Is_Type (T1)
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
                     (T1 = Universal_Access
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

      --  Coverage for incomplete types

      elsif Ekind (T1) = E_Incomplete_Type
        and then Present (Full_View (T1))
      then
         return Covers (Full_View (T1), T2);

      elsif Ekind (T2) = E_Incomplete_Type
        and then Present (Full_View (T2))
      then
         return Covers (T1, Full_View (T2));

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
        and then Is_Anonymous_Access_Type (T1)
        and then Is_Anonymous_Access_Type (T2)
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

      function Is_Ambiguous_Boolean_Operator (I : Interp) return Boolean;
      --  Determine whether I corresponds to an "ambiguous" boolean operator.
      --  Such an interpretation is used to record the ambiguity of operands
      --  diagnosed during the analysis of comparison and equality operations.
      --  See Find_Comparison_Equality_Types in Sem_Ch4 for the rationale.

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

      function Remove_Conversions_And_Abstract_Operations return Interp;
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

      function Is_User_Defined_Anonymous_Access_Equality
        (User_Subp, Predef_Subp : Entity_Id) return Boolean;
      --  Check for Ada 2005, AI-020: If the context involves an anonymous
      --  access operand, recognize a user-defined equality (User_Subp) with
      --  the proper signature, declared in the same declarative list as the
      --  type and not hiding a predefined equality Predef_Subp.

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
             (Is_Package_Or_Generic_Package (Scop)
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

           --  Determine if the renaming came from source or was generated as a
           --  a result of generic expansion since the actual is represented by
           --  a constructed subprogram renaming.

           and then not Comes_From_Source (Unit_Declaration_Node (S))

           and then
             (Is_Generic_Instance (Scope (S))
               or else Is_Wrapper_Package (Scope (S)));
      end Is_Actual_Subprogram;

      -----------------------------------
      -- Is_Ambiguous_Boolean_Operator --
      -----------------------------------

      function Is_Ambiguous_Boolean_Operator (I : Interp) return Boolean is
      begin
         return Ekind (I.Nam) = E_Operator
           and then I.Typ = Standard_Boolean
           and then I.Opnd_Typ = Any_Type;
      end Is_Ambiguous_Boolean_Operator;

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

            --  Formal_Typ is a private view, or Opnd_Typ and Formal_Typ are
            --  compatible only on a base-type basis.

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

      ------------------------------------------------
      -- Remove_Conversions_And_Abstract_Operations --
      ------------------------------------------------

      function Remove_Conversions_And_Abstract_Operations return Interp is
         I    : Interp_Index;
         It   : Interp;
         It1  : Interp;
         F1   : Entity_Id;
         Act1 : Node_Id;
         Act2 : Node_Id;

         function Has_Abstract_Interpretation (N : Node_Id) return Boolean;
         --  If an operation has universal operands, the universal operation
         --  is present among its interpretations. If there is an abstract
         --  interpretation for the operator, with a numeric result, this
         --  interpretation was already removed in sem_ch4, but the universal
         --  one is still visible. We must rescan the list of operators and
         --  remove the universal interpretation to resolve the ambiguity.

         function Is_Numeric_Only_Type (T : Entity_Id) return Boolean;
         --  Return True if T is a numeric type and not Any_Type

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
                    and then Is_Numeric_Only_Type (Etype (E))
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

         --------------------------
         -- Is_Numeric_Only_Type --
         --------------------------

         function Is_Numeric_Only_Type (T : Entity_Id) return Boolean is
         begin
            return Is_Numeric_Type (T) and then T /= Any_Type;
         end Is_Numeric_Only_Type;

      --  Start of processing for Remove_Conversions_And_Abstract_Operations

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
                     or else Nkind (Left_Opnd (Act1)) in
                               N_Integer_Literal | N_Real_Literal)
                 and then Nkind (Right_Opnd (Act1)) in
                            N_Integer_Literal | N_Real_Literal
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
                    and then Nkind (Right_Opnd (Act2)) in
                               N_Integer_Literal | N_Real_Literal
                    and then Has_Compatible_Type (Act2, Standard_Boolean)
                  then
                     --  The preference rule on the first actual is not
                     --  sufficient to disambiguate.

                     goto Next_Interp;

                  else
                     It1 := It;
                  end if;

               elsif Is_Numeric_Only_Type (Etype (F1))
                 and then Has_Abstract_Interpretation (Act1)
               then
                  --  Current interpretation is not the right one because it
                  --  expects a numeric operand. Examine all the others.

                  declare
                     I  : Interp_Index;
                     It : Interp;

                  begin
                     Get_First_Interp (N, I, It);
                     while Present (It.Typ) loop
                        if not Is_Numeric_Only_Type
                                 (Etype (First_Formal (It.Nam)))
                        then
                           if No (Act2)
                             or else not
                               Is_Numeric_Only_Type
                                 (Etype (Next_Formal (First_Formal (It.Nam))))
                             or else not Has_Abstract_Interpretation (Act2)
                           then
                              return It;
                           end if;
                        end if;

                        Get_Next_Interp (I, It);
                     end loop;

                     return No_Interp;
                  end;

               elsif Is_Numeric_Only_Type (Etype (F1))
                 and then Present (Act2)
                 and then Has_Abstract_Interpretation (Act2)
               then
                  --  Current interpretation is not the right one because it
                  --  expects a numeric operand. Examine all the others.

                  declare
                     I  : Interp_Index;
                     It : Interp;

                  begin
                     Get_First_Interp (N, I, It);
                     while Present (It.Typ) loop
                        if not Is_Numeric_Only_Type
                                (Etype (Next_Formal (First_Formal (It.Nam))))
                        then
                           if not Is_Numeric_Only_Type
                                    (Etype (First_Formal (It.Nam)))
                             or else not Has_Abstract_Interpretation (Act1)
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

         return It1;
      end Remove_Conversions_And_Abstract_Operations;

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

      -----------------------------------------------
      -- Is_User_Defined_Anonymous_Access_Equality --
      -----------------------------------------------

      function Is_User_Defined_Anonymous_Access_Equality
        (User_Subp, Predef_Subp : Entity_Id) return Boolean is
      begin
         return Present (User_Subp)

         --  Check for Ada 2005 and use of anonymous access

           and then Ada_Version >= Ada_2005
           and then Etype (User_Subp) = Standard_Boolean
           and then Is_Anonymous_Access_Type (Operand_Type)

         --  This check is only relevant if User_Subp is visible and not in
         --  an instance

           and then (In_Open_Scopes (Scope (User_Subp))
                      or else Is_Potentially_Use_Visible (User_Subp))
           and then not In_Instance
           and then not Hides_Op (User_Subp, Predef_Subp)

         --  Is User_Subp declared in the same declarative list as the type?

           and then
             In_Same_Declaration_List
               (Designated_Type (Operand_Type),
                Unit_Declaration_Node (User_Subp));
      end Is_User_Defined_Anonymous_Access_Equality;

   --  Start of processing for Disambiguate

   begin
      --  Recover the two legal interpretations

      Get_First_Interp (N, I, It);
      while I /= I1 loop
         Get_Next_Interp (I, It);
      end loop;

      It1  := It;
      Nam1 := It.Nam;

      --  Return immediately if either corresponds to a recorded ambiguity

      if Is_Ambiguous_Boolean_Operator (It1) then
         return It1;
      end if;

      while I /= I2 loop
         Get_Next_Interp (I, It);
      end loop;

      It2  := It;
      Nam2 := It.Nam;

      --  See above

      if Is_Ambiguous_Boolean_Operator (It2) then
         return It2;
      end if;

      --  Check whether one of the entities is an Ada 2005/2012/2022 and we
      --  are operating in an earlier mode, in which case we discard the Ada
      --  2005/2012/2022 entity, so that we get proper Ada 95 overload
      --  resolution.

      if Ada_Version < Ada_2005 then
         if Is_Ada_2005_Only (Nam1)
           or else Is_Ada_2012_Only (Nam1)
           or else Is_Ada_2022_Only (Nam1)
         then
            return It2;

         elsif Is_Ada_2005_Only (Nam2)
           or else Is_Ada_2012_Only (Nam2)
           or else Is_Ada_2022_Only (Nam2)
         then
            return It1;
         end if;

      --  Check whether one of the entities is an Ada 2012/2022 entity and we
      --  are operating in Ada 2005 mode, in which case we discard the Ada 2012
      --  Ada 2022 entity, so that we get proper Ada 2005 overload resolution.

      elsif Ada_Version = Ada_2005 then
         if Is_Ada_2012_Only (Nam1) or else Is_Ada_2022_Only (Nam1) then
            return It2;
         elsif Is_Ada_2012_Only (Nam2) or else Is_Ada_2022_Only (Nam2) then
            return It1;
         end if;

      --  Ditto for Ada 2012 vs Ada 2022.

      elsif Ada_Version = Ada_2012 then
         if Is_Ada_2022_Only (Nam1) then
            return It2;
         elsif Is_Ada_2022_Only (Nam2) then
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
                  if Is_Universal_Numeric_Type (It.Typ)
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

               if Present (Arg2) then
                  if Ekind (Nam1) = E_Operator then
                     Predef_Subp := Nam1;
                     User_Subp   := Nam2;
                  elsif Ekind (Nam2) = E_Operator then
                     Predef_Subp := Nam2;
                     User_Subp   := Nam1;
                  else
                     Predef_Subp := Empty;
                     User_Subp   := Empty;
                  end if;

                  --  Take into account universal interpretation as well as
                  --  universal_access equality, as long as AI05-0020 does not
                  --  trigger.

                  if (Present (Universal_Interpretation (Arg1))
                       and then Universal_Interpretation (Arg2) =
                                Universal_Interpretation (Arg1))
                    or else
                      (Nkind (N) in N_Op_Eq | N_Op_Ne
                        and then (Is_Anonymous_Access_Type (Etype (Arg1))
                                    or else
                                  Is_Anonymous_Access_Type (Etype (Arg2)))
                        and then not
                          Is_User_Defined_Anonymous_Access_Equality
                            (User_Subp, Predef_Subp))
                  then
                     Get_First_Interp (N, I, It);
                     while Scope (It.Nam) /= Standard_Standard loop
                        Get_Next_Interp (I, It);
                     end loop;

                     return It;
                  end if;
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
         if Is_Anonymous_Access_Type (It1.Typ) then
            if Ekind (It2.Typ) = Ekind (It1.Typ) then

               --  True ambiguity

               return No_Interp;

            else
               return It1;
            end if;

         elsif Is_Anonymous_Access_Type (It2.Typ) then
            return It2;

         --  No legal interpretation

         else
            return No_Interp;
         end if;

      --  Two access attribute types may have been created for an expression
      --  with an implicit dereference, which is automatically overloaded.
      --  If both access attribute types designate the same object type,
      --  disambiguation if any will take place elsewhere, so keep any one of
      --  the interpretations.

      elsif Ekind (It1.Typ) = E_Access_Attribute_Type
        and then Ekind (It2.Typ) = E_Access_Attribute_Type
        and then Designated_Type (It1.Typ) = Designated_Type (It2.Typ)
      then
         return It1;

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
               return Remove_Conversions_And_Abstract_Operations;
            end if;
         else
            return Remove_Conversions_And_Abstract_Operations;
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

      --  If the user-defined operator matches the signature of the operator,
      --  and is declared in an open scope, or in the scope of the resulting
      --  type, or given by an expanded name that names its scope, it hides
      --  the predefined operator for the type. But exponentiation has to be
      --  special-cased because the latter operator does not have a symmetric
      --  signature, and may not be hidden by the explicit one.

      elsif Hides_Op (User_Subp, Predef_Subp)
        or else (Nkind (N) = N_Function_Call
                  and then Nkind (Name (N)) = N_Expanded_Name
                  and then (Chars (Predef_Subp) /= Name_Op_Expon
                             or else Hides_Op (User_Subp, Predef_Subp))
                  and then Scope (User_Subp) = Entity (Prefix (Name (N))))
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
              and then Chars (Nam1) in Name_Op_Multiply | Name_Op_Divide
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

            --  Check for AI05-020

            elsif Chars (Nam1) in Name_Op_Eq | Name_Op_Ne
              and then Is_User_Defined_Anonymous_Access_Equality
                         (User_Subp, Predef_Subp)
            then
               if It2.Nam = Predef_Subp then
                  return It1;
               else
                  return It2;
               end if;

            --  RM 8.4(10): an immediately visible operator hides a use-visible
            --  user-defined operation that is a homograph. This disambiguation
            --  cannot take place earlier because visibility of the predefined
            --  operator can only be established when operand types are known.

            elsif Ekind (User_Subp) = E_Function
              and then Ekind (Predef_Subp) = E_Operator
              and then Operator_Matches_Spec (Predef_Subp, User_Subp)
              and then Nkind (N) in N_Op
              and then not Is_Overloaded (Right_Opnd (N))
              and then
                Is_Immediately_Visible (Base_Type (Etype (Right_Opnd (N))))
              and then Is_Potentially_Use_Visible (User_Subp)
            then
               if It1.Nam = Predef_Subp then
                  return It1;
               else
                  return It2;
               end if;

            else
               return Remove_Conversions_And_Abstract_Operations;
            end if;

         elsif It1.Nam = Predef_Subp then
            return It1;

         else
            return It2;
         end if;
      end if;
   end Disambiguate;

   -------------------------
   -- Entity_Matches_Spec --
   -------------------------

   function Entity_Matches_Spec (Old_S, New_S : Entity_Id) return Boolean is
   begin
      --  For the simple case of same kinds, type conformance is required, but
      --  a parameterless function can also rename a literal.

      if Ekind (Old_S) = Ekind (New_S)
        or else (Ekind (New_S) = E_Function
                  and then Ekind (Old_S) = E_Enumeration_Literal)
      then
         return Type_Conformant (New_S, Old_S);

      --  Likewise for a procedure and an entry

      elsif Ekind (New_S) = E_Procedure and then Is_Entry (Old_S) then
         return Type_Conformant (New_S, Old_S);

      --  For a user-defined operator, use the dedicated predicate

      elsif Ekind (New_S) = E_Function and then Ekind (Old_S) = E_Operator then
         return Operator_Matches_Spec (Old_S, New_S);

      else
         return False;
      end if;
   end Entity_Matches_Spec;

   ----------------------
   -- Find_Unique_Type --
   ----------------------

   function Find_Unique_Type (L : Node_Id; R : Node_Id) return Entity_Id is
      T  : constant Entity_Id := Specific_Type (Etype (L), Etype (R));

   begin
      if T = Any_Type then
         if Is_User_Defined_Literal (L, Etype (R)) then
            return Etype (R);
         elsif Is_User_Defined_Literal (R, Etype (L)) then
            return Etype (L);
         end if;
      end if;

      return T;
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
      if Is_Overloaded (N) then
         --  Move through the formals and actuals of the call to
         --  determine if an abstract interpretation exists.

         Act_Parm  := First_Actual (N);
         Form_Parm := First_Formal (E);
         while Present (Act_Parm) and then Present (Form_Parm) loop
            Act := Act_Parm;

            --  Extract the actual from a parameter association

            if Nkind (Act) = N_Parameter_Association then
               Act := Explicit_Actual_Parameter (Act);
            end if;

            --  Use the actual and the type of its correponding formal to test
            --  for an abstract interpretation and return it when found.

            Abstr_Op := Has_Abstract_Op (Act, Etype (Form_Parm));

            if Present (Abstr_Op) then
               return Abstr_Op;
            end if;

            Next_Actual (Act_Parm);
            Next_Formal (Form_Parm);
         end loop;
      end if;

      --  Otherwise, return empty

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

      Int_Ind := Interp_Map.Get (O_N);

      --  Procedure should never be called if the node has no interpretations

      if Int_Ind < 0 then
         raise Program_Error;
      end if;

      I  := Int_Ind;
      It := All_Interp.Table (Int_Ind);
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

   function Has_Compatible_Type (N : Node_Id; Typ : Entity_Id) return Boolean
   is
      I  : Interp_Index;
      It : Interp;

   begin
      if N = Error then
         return False;
      end if;

      if Nkind (N) = N_Subtype_Indication or else not Is_Overloaded (N) then
         if Covers (Typ, Etype (N))

            --  Ada 2005 (AI-345): The context may be a synchronized interface.
            --  If the type is already frozen, use the corresponding_record to
            --  check whether it is a proper descendant.

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

           or else Is_User_Defined_Literal (N, Typ)

         then
            return True;
         end if;

      --  Overloaded case

      else
         Get_First_Interp (N, I, It);
         while Present (It.Typ) loop
            if Covers (Typ, It.Typ)

               --  Ada 2005 (AI-345)

              or else
                (Is_Record_Type (Typ)
                  and then Is_Concurrent_Type (It.Typ)
                  and then Present (Corresponding_Record_Type (Etype (It.Typ)))
                  and then
                    Covers (Typ, Corresponding_Record_Type (Etype (It.Typ))))

              or else
                (Is_Concurrent_Type (Typ)
                  and then Is_Record_Type (It.Typ)
                  and then Present (Corresponding_Record_Type (Typ))
                  and then
                    Covers (Corresponding_Record_Type (Typ), Etype (It.Typ)))

            then
               return True;
            end if;

            Get_Next_Interp (I, It);
         end loop;
      end if;

      return False;
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

   function Hash (N : Node_Id) return Header_Num is
   begin
      return Header_Num (N mod Header_Max);
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
      Interp_Map.Reset;
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
            if Is_Record_Type (E)
              and then Present (Interfaces (E))
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
      --  list of interfaces (available in the parent of the concurrent type).

      if Is_Concurrent_Type (Target_Typ) then
         declare
            AI : Node_Id;

         begin
            AI := First (Interface_List (Parent (Target_Typ)));

            --  The progenitor itself may be a subtype of an interface type

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
            --  In a spec expression or in an expression function, the use of
            --  an incomplete type is legal; legality of the conversion will be
            --  checked at freeze point of related entity.

            if In_Spec_Expression then
               return True;

            else
               pragma Assert (Present (Non_Limited_View (Target_Typ)));
               Target_Typ := Non_Limited_View (Target_Typ);
            end if;
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
         return
           Nkind (Par) = N_Object_Declaration
             and then Present (Corresponding_Generic_Association (Par));

      elsif Nkind (Par) = N_Object_Renaming_Declaration then
         return Present (Corresponding_Generic_Association (Par));

      elsif Nkind (Par) in N_Statement_Other_Than_Procedure_Call then
         return False;

      else
         return In_Generic_Actual (Par);
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
               --  Use the full-view of private types (if allowed). Guard
               --  against infinite loops when full view has same type as
               --  parent, as can happen with interface extensions.

               if Use_Full_View
                 and then Is_Private_Type (Par)
                 and then Present (Full_View (Par))
                 and then Par /= Etype (Full_View (Par))
               then
                  Par := Etype (Full_View (Par));
               else
                  Par := Etype (Par);
               end if;
            end if;
         end loop;
      end if;
   end Is_Ancestor;

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

   -------------------------
   -- Is_Visible_Operator --
   -------------------------

   function Is_Visible_Operator (N : Node_Id; Typ : Entity_Id) return Boolean
   is
   begin
      --  The predefined operators of the universal types are always visible

      if Typ in Universal_Integer | Universal_Real | Universal_Access then
         return True;

      --  AI95-0230: Keep restriction imposed by Ada 83 and 95, do not allow
      --  anonymous access types in universal_access equality operators.

      elsif Is_Anonymous_Access_Type (Typ) then
         return Ada_Version >= Ada_2005;

      --  Similar reasoning for special types used for composite types before
      --  type resolution is done.

      elsif Typ = Any_Composite or else Typ = Any_String then
         return True;

      --  Within an instance, the predefined operators of the formal types are
      --  visible and, for the other types, the generic package declaration has
      --  already been successfully analyzed. Likewise for an inlined body.

      elsif In_Instance or else In_Inlined_Body then
         return True;

     --  If the operation is given in functional notation and the prefix is an
     --  expanded name, then the operator is visible if the prefix is the scope
     --  of the type, or System if the type is declared in an extension of it.

      elsif Nkind (N) = N_Function_Call
        and then Nkind (Name (N)) = N_Expanded_Name
      then
         declare
            Pref : constant Entity_Id := Entity (Prefix (Name (N)));
            Scop : constant Entity_Id := Scope (Typ);

         begin
            return Pref = Scop
              or else (Present (System_Aux_Id)
                        and then Scop = System_Aux_Id
                        and then Pref = Scope (Scop));
         end;

      --  Otherwise the operator is visible when the type is visible

      else
         return Is_Potentially_Use_Visible (Typ)
           or else In_Use (Typ)
           or else (In_Use (Scope (Typ)) and then not Is_Hidden (Typ))
           or else In_Open_Scopes (Scope (Typ));
      end if;
   end Is_Visible_Operator;

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
   begin
      All_Interp.Append (No_Interp);

      --  Add or rewrite the existing node
      Last_Overloaded := N;
      Interp_Map.Set (N, All_Interp.Last);
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

         if Op_Name in Name_Op_Subtract | Name_Op_Add | Name_Op_Abs then
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

         if Op_Name in Name_Op_And | Name_Op_Or | Name_Op_Xor then
            return Base_Type (T1) = Base_Type (T2)
              and then Base_Type (T1) = Base_Type (T)
              and then Valid_Boolean_Arg (Base_Type (T));

         elsif Op_Name in Name_Op_Eq | Name_Op_Ne then
            return Base_Type (T1) = Base_Type (T2)
              and then Valid_Equality_Arg (T1)
              and then Is_Boolean_Type (T);

         elsif Op_Name in Name_Op_Lt | Name_Op_Le | Name_Op_Gt | Name_Op_Ge
         then
            return Base_Type (T1) = Base_Type (T2)
              and then Valid_Comparison_Arg (T1)
              and then Is_Boolean_Type (T);

         elsif Op_Name in Name_Op_Add | Name_Op_Subtract then
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

         elsif Op_Name in Name_Op_Mod | Name_Op_Rem then
            return Base_Type (T1) = Base_Type (T2)
              and then Base_Type (T1) = Base_Type (T)
              and then Is_Integer_Type (T);

         elsif Op_Name = Name_Op_Expon then
            return Base_Type (T1) = Base_Type (T)
              and then Is_Numeric_Type (T)
              and then Base_Type (T2) = Base_Type (Standard_Integer);

         elsif Op_Name = Name_Op_Concat then
            return Is_Array_Type (T)
              and then Base_Type (T) = Base_Type (Etype (Op))
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
      Old_Ind : Interp_Index;
      O_N     : Node_Id;

   begin
      if Is_Overloaded (Old_N) then
         Set_Is_Overloaded (New_N);

         if Nkind (Old_N) = N_Selected_Component
           and then Is_Overloaded (Selector_Name (Old_N))
         then
            O_N := Selector_Name (Old_N);
         else
            O_N := Old_N;
         end if;

         Old_Ind := Interp_Map.Get (O_N);
         pragma Assert (Old_Ind >= 0);

         New_Interps (New_N);
         Interp_Map.Set (New_N, Old_Ind);
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
        or else (T1 = Any_Modular       and then Is_Modular_Integer_Type (T2))
        or else (T1 = Any_Character     and then Is_Character_Type (T2))
        or else (T1 = Any_String        and then Is_String_Type (T2))
        or else (T1 = Any_Composite     and then Is_Aggregate_Type (T2))
      then
         return B2;

      elsif (T1 = Universal_Access
              or else Ekind (T1) in E_Allocator_Type | E_Access_Attribute_Type)
        and then (Is_Access_Type (T2) or else Is_Remote_Access (T2))
      then
         return B2;

      elsif T1 = Raise_Type then
         return B2;

      elsif     (T2 = Universal_Integer and then Is_Integer_Type (T1))
        or else (T2 = Universal_Real    and then Is_Real_Type (T1))
        or else (T2 = Universal_Fixed   and then Is_Fixed_Point_Type (T1))
        or else (T2 = Any_Fixed         and then Is_Fixed_Point_Type (T1))
        or else (T2 = Any_Modular       and then Is_Modular_Integer_Type (T1))
        or else (T2 = Any_Character     and then Is_Character_Type (T1))
        or else (T2 = Any_String        and then Is_String_Type (T1))
        or else (T2 = Any_Composite     and then Is_Aggregate_Type (T1))
      then
         return B1;

      elsif (T2 = Universal_Access
              or else Ekind (T2) in E_Allocator_Type | E_Access_Attribute_Type)
        and then (Is_Access_Type (T1) or else Is_Remote_Access (T1))
      then
         return B1;

      elsif T2 = Raise_Type then
         return B1;

      --  Ada 2005 (AI-251): T1 and T2 are class-wide types, and T2 is an
      --  interface, return T1, and vice versa.

      elsif Is_Class_Wide_Type (T1)
        and then Is_Class_Wide_Type (T2)
        and then Is_Interface (Etype (T2))
      then
         return B1;

      elsif Is_Class_Wide_Type (T2)
        and then Is_Class_Wide_Type (T1)
        and then Is_Interface (Etype (T1))
      then
         return B2;

      --  Ada 2005 (AI-251): T1 is a concrete type that implements the
      --  class-wide interface T2, return T1, and vice versa.

      elsif Is_Tagged_Type (T1)
        and then Is_Class_Wide_Type (T2)
        and then Is_Interface (Etype (T2))
        and then Interface_Present_In_Ancestor (Typ   => T1,
                                                Iface => Etype (T2))
      then
         return B1;

      elsif Is_Tagged_Type (T2)
        and then Is_Class_Wide_Type (T1)
        and then Is_Interface (Etype (T1))
        and then Interface_Present_In_Ancestor (Typ   => T2,
                                                Iface => Etype (T1))
      then
         return B2;

      elsif Is_Class_Wide_Type (T1)
        and then Is_Ancestor (Root_Type (T1), T2)
      then
         return B1;

      elsif Is_Class_Wide_Type (T2)
        and then Is_Ancestor (Root_Type (T2), T1)
      then
         return B2;

      elsif Is_Access_Type (T1)
        and then Is_Access_Type (T2)
        and then Is_Class_Wide_Type (Designated_Type (T1))
        and then not Is_Class_Wide_Type (Designated_Type (T2))
        and then
          Is_Ancestor (Root_Type (Designated_Type (T1)), Designated_Type (T2))
      then
         return T1;

      elsif Is_Access_Type (T1)
        and then Is_Access_Type (T2)
        and then Is_Class_Wide_Type (Designated_Type (T2))
        and then not Is_Class_Wide_Type (Designated_Type (T1))
        and then
          Is_Ancestor (Root_Type (Designated_Type (T2)), Designated_Type (T1))
      then
         return T2;

      elsif Ekind (B1) in E_Access_Subprogram_Type
                        | E_Access_Protected_Subprogram_Type
        and then Ekind (Designated_Type (B1)) /= E_Subprogram_Type
        and then Is_Access_Type (T2)
      then
         return T2;

      elsif Ekind (B2) in E_Access_Subprogram_Type
                        | E_Access_Protected_Subprogram_Type
        and then Ekind (Designated_Type (B2)) /= E_Subprogram_Type
        and then Is_Access_Type (T1)
      then
         return T1;

      --  Ada 2005 (AI-230): Support the following operators:

      --    function "="  (L, R : universal_access) return Boolean;
      --    function "/=" (L, R : universal_access) return Boolean;

      --  Pool-specific access types (E_Access_Type) are not covered by these
      --  operators because of the legality rule of 4.5.2(9.2): "The operands
      --  of the equality operators for universal_access shall be convertible
      --  to one another (see 4.6)". For example, considering the type decla-
      --  ration "type P is access Integer" and an anonymous access to Integer,
      --  P is convertible to "access Integer" by 4.6 (24.11-24.15), but there
      --  is no rule in 4.6 that allows "access Integer" to be converted to P.
      --  Note that this does not preclude one operand to be a pool-specific
      --  access type, as a previous version of this code enforced.

      elsif Is_Anonymous_Access_Type (T1)
        and then Is_Access_Type (T2)
        and then Ada_Version >= Ada_2005
      then
         return T1;

      elsif Is_Anonymous_Access_Type (T2)
        and then Is_Access_Type (T1)
        and then Ada_Version >= Ada_2005
      then
         return T2;

      --  With types exported from instantiation, also check private views the
      --  same way as Covers

      elsif Is_Private_Type (T1) and then Is_Generic_Actual_Type (T2) then
         if Present (Full_View (T1)) then
            return Specific_Type (Full_View (T1), T2);

         elsif Present (Underlying_Full_View (T1)) then
            return Specific_Type (Underlying_Full_View (T1), T2);
         end if;

      elsif Is_Private_Type (T2) and then Is_Generic_Actual_Type (T1) then
         if Present (Full_View (T2)) then
            return Specific_Type (T1, Full_View (T2));

         elsif Present (Underlying_Full_View (T2)) then
            return Specific_Type (T1, Underlying_Full_View (T2));
         end if;
      end if;

      --  If none of the above cases applies, types are not compatible

      return Any_Type;
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
        or else T = Raise_Type
      then
         return True;

      elsif Is_Array_Type (T)
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

   --  See above for the reason why aggregates and strings are included

   function Valid_Comparison_Arg (T : Entity_Id) return Boolean is
   begin
      if Is_Discrete_Type (T) or else Is_Real_Type (T) then
         return True;

      elsif T = Any_Composite or else T = Any_String then
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

   ------------------------
   -- Valid_Equality_Arg --
   ------------------------

   --  Same reasoning as above but implicit because of the nonlimited test

   function Valid_Equality_Arg (T : Entity_Id) return Boolean is
   begin
      --  AI95-0230: Keep restriction imposed by Ada 83 and 95, do not allow
      --  anonymous access types in universal_access equality operators.

      if Is_Anonymous_Access_Type (T) then
         return Ada_Version >= Ada_2005;

      elsif Is_Incomplete_Type (T) then
         return False;

      elsif not Is_Limited_Type (T) then
         return True;

      elsif Is_Array_Type (T)
        and then not Is_Limited_Type (Component_Type (T))
        and then Available_Full_View_Of_Component (T)
      then
         return True;

      else
         return False;
      end if;
   end Valid_Equality_Arg;

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
         if Is_Entity_Name (N) then
            Write_Line ("Non-overloaded entity ");
            Write_Entity_Info (Entity (N), " ");
         end if;

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
