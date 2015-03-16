------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ U N S T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2014-2015, Free Software Foundation, Inc.         --
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
with Einfo;    use Einfo;
with Elists;   use Elists;
with Exp_Util; use Exp_Util;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Rtsfind;  use Rtsfind;
with Sinput;   use Sinput;
with Sem;      use Sem;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Mech; use Sem_Mech;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Table;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Exp_Unst is

   --  Tables used by Unnest_Subprogram

   type Subp_Entry is record
      Ent : Entity_Id;
      --  Entity of the subprogram

      Bod : Node_Id;
      --  Subprogram_Body node for this subprogram

      Lev : Nat;
      --  Subprogram level (1 = outer subprogram (Subp argument), 2 = nested
      --  immediately within this outer subprogram etc.)

      Urefs : Elist_Id;
      --  This is a copy of the Uplevel_References field from the entity for
      --  the subprogram. Copy this to reuse the field for Subps_Index.

      ARECnF : Entity_Id;
      --  This entity is defined for all subprograms with uplevel references
      --  except for the top-level subprogram (Subp itself). It is the entity
      --  for the formal which is added to the parameter list to pass the
      --  pointer to the activation record. Note that for this entity, n is
      --  one less than the current level.

      ARECn   : Entity_Id;
      ARECnT  : Entity_Id;
      ARECnPT : Entity_Id;
      ARECnP  : Entity_Id;
      --  These AREC entities are defined only for subprograms for which we
      --  generate an activation record declaration, i.e. for subprograms
      --  with at least one nested subprogram that have uplevel referennces.
      --  They are set to Empty for all other cases.

      ARECnU : Entity_Id;
      --  This AREC entity is the uplink component. It is other than Empty only
      --  for nested subprograms that themselves have nested subprograms and
      --  have uplevel references. Note that the n here is one less than the
      --  level of the subprogram defining the activation record.

   end record;

   subtype SI_Type is Nat;

   package Subps is new Table.Table (
     Table_Component_Type => Subp_Entry,
     Table_Index_Type     => SI_Type,
     Table_Low_Bound      => 1,
     Table_Initial        => 100,
     Table_Increment      => 200,
     Table_Name           => "Unnest_Subps");
   --  Records the subprograms in the nest whose outer subprogram is Subp

   type Call_Entry is record
      N : Node_Id;
      --  The actual call

      From : Entity_Id;
      --  Entity of the subprogram containing the call

      To : Entity_Id;
      --  Entity of the subprogram called
   end record;

   package Calls is new Table.Table (
     Table_Component_Type => Call_Entry,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => 100,
     Table_Increment      => 200,
     Table_Name           => "Unnest_Calls");
   --  Records each call within the outer subprogram and all nested subprograms
   --  that are to other subprograms nested within the outer subprogram. These
   --  are the calls that may need an additional parameter.

   -------------------------------------
   -- Check_Uplevel_Reference_To_Type --
   -------------------------------------

   procedure Check_Uplevel_Reference_To_Type (Typ : Entity_Id) is
      function Check_Dynamic_Type (T : Entity_Id) return Boolean;
      --  This is an internal recursive routine that checks if T or any of
      --  its subsdidiary types are dynamic. If so, then the original Typ is
      --  marked as having an uplevel reference, as is the subsidiary type in
      --  question, and any referenced dynamic bounds are also marked as having
      --  an uplevel reference, and True is returned. If the type is a static
      --  type, then False is returned;

      ------------------------
      -- Check_Dynamic_Type --
      ------------------------

      function Check_Dynamic_Type (T : Entity_Id) return Boolean is
         DT : Boolean := False;

      begin
         --  If it's a static type, nothing to do

         if Is_Static_Type (T) then
            return False;

         --  If the type is uplevel referenced, then it must be dynamic

         elsif Has_Uplevel_Reference (T) then
            Set_Has_Uplevel_Reference (Typ);
            return True;

         --  If the type is at library level, always consider it static, since
         --  uplevel references do not matter in this case.

         elsif Is_Library_Level_Entity (T) then
            Set_Is_Static_Type (T);
            return False;

         --  Otherwise we need to figure out what the story is with this type

         else
            DT := False;

            --  For a scalar type, check bounds

            if Is_Scalar_Type (T) then

               --  If both bounds static, then this is a static type

               declare
                  LB : constant Node_Id := Type_Low_Bound (T);
                  UB : constant Node_Id := Type_High_Bound (T);

               begin
                  if not Is_Static_Expression (LB) then
                     Set_Has_Uplevel_Reference (Entity (LB));
                     DT := True;
                  end if;

                  if not Is_Static_Expression (UB) then
                     Set_Has_Uplevel_Reference (Entity (UB));
                     DT := True;
                  end if;
               end;

            --  For record type, check all components

            elsif Is_Record_Type (T) then
               declare
                  C : Entity_Id;

               begin
                  C := First_Component_Or_Discriminant (T);
                  while Present (C) loop
                     if Check_Dynamic_Type (Etype (C)) then
                        DT := True;
                     end if;

                     Next_Component_Or_Discriminant (C);
                  end loop;
               end;

            --  For array type, check index types and component type

            elsif Is_Array_Type (T) then
               declare
                  IX : Node_Id;

               begin
                  if Check_Dynamic_Type (Component_Type (T)) then
                     DT := True;
                  end if;

                  IX := First_Index (T);
                  while Present (IX) loop
                     if Check_Dynamic_Type (Etype (IX)) then
                        DT := True;
                     end if;

                     Next_Index (IX);
                  end loop;
               end;

            --  For now, ignore other types

            else
               return False;
            end if;

            --  See if we marked that type as dynamic

            if DT then
               Set_Has_Uplevel_Reference (T);
               Set_Has_Uplevel_Reference (Typ);
               return True;

            --  If not mark it as static

            else
               Set_Is_Static_Type (T);
               return False;
            end if;
         end if;
      end Check_Dynamic_Type;

   --  Start of processing for Check_Uplevel_Reference_To_Type

   begin
      --  Nothing to do inside a generic (all processing is for instance)

      if Inside_A_Generic then
         return;

      --  Nothing to do if we know this is a static type

      elsif Is_Static_Type (Typ) then
         return;

      --  Nothing to do if already marked as uplevel referenced

      elsif Has_Uplevel_Reference (Typ) then
         return;

      --  Otherwise check if we have a dynamic type

      else
         if Check_Dynamic_Type (Typ) then
            Set_Has_Uplevel_Reference (Typ);
         end if;
      end if;

      null;
   end Check_Uplevel_Reference_To_Type;

   ----------------------------
   -- Note_Uplevel_Reference --
   ----------------------------

   procedure Note_Uplevel_Reference (N : Node_Id; Subp : Entity_Id) is
      Elmt : Elmt_Id;

   begin
      --  Nothing to do inside a generic (all processing is for instance)

      if Inside_A_Generic then
         return;
      end if;

      --  Nothing to do if reference has no entity field

      if Nkind (N) not in N_Has_Entity then
         return;
      end if;

      --  Establish list if first call for Uplevel_References

      if No (Uplevel_References (Subp)) then
         Set_Uplevel_References (Subp, New_Elmt_List);
      end if;

      --  Ignore if node is already in the list. This is a bit inefficient,
      --  but we can definitely get duplicates that cause trouble!

      Elmt := First_Elmt (Uplevel_References (Subp));
      while Present (Elmt) loop
         if N = Node (Elmt) then
            return;
         else
            Next_Elmt (Elmt);
         end if;
      end loop;

      --  Add new entry to Uplevel_References. Each entry is two elements of
      --  the list. The first is the actual reference, the second is the
      --  enclosing subprogram at the point of reference

      Append_Elmt (N, Uplevel_References (Subp));

      if Is_Subprogram (Current_Scope) then
         Append_Elmt (Current_Scope, Uplevel_References (Subp));
      else
         Append_Elmt
           (Enclosing_Subprogram (Current_Scope), Uplevel_References (Subp));
      end if;

      Set_Has_Uplevel_Reference (Entity (N));
      Set_Has_Uplevel_Reference (Subp);
   end Note_Uplevel_Reference;

   -----------------------
   -- Unnest_Subprogram --
   -----------------------

   procedure Unnest_Subprogram (Subp : Entity_Id; Subp_Body : Node_Id) is
      function Actual_Ref (N : Node_Id) return Node_Id;
      --  This function is applied to an element in the Uplevel_References
      --  list, and it finds the actual reference. Often this is just N itself,
      --  but in some cases it gets rewritten, e.g. as a Type_Conversion, and
      --  this function digs out the actual reference

      function AREC_String (Lev : Pos) return String;
      --  Given a level value, 1, 2, ... returns the string AREC, AREC2, ...

      function Enclosing_Subp (Subp : SI_Type) return SI_Type;
      --  Subp is the index of a subprogram which has a Lev greater than 1.
      --  This function returns the index of the enclosing subprogram which
      --  will have a Lev value one less than this.

      function Get_Level (Sub : Entity_Id) return Nat;
      --  Sub is either Subp itself, or a subprogram nested within Subp. This
      --  function returns the level of nesting (Subp = 1, subprograms that
      --  are immediately nested within Subp = 2, etc).

      function Subp_Index (Sub : Entity_Id) return SI_Type;
      --  Given the entity for a subprogram, return corresponding Subps index

      ----------------
      -- Actual_Ref --
      ----------------

      function Actual_Ref (N : Node_Id) return Node_Id is
      begin
         case Nkind (N) is

            --  If we have an entity reference, then this is the actual ref

            when N_Has_Entity =>
               return N;

            --  For a type conversion, go get the expression

            when N_Type_Conversion =>
               return Expression (N);

            --  For an explicit dereference, get the prefix

            when N_Explicit_Dereference =>
               return Prefix (N);

            --  No other possibilities should exist

            when others =>
               raise Program_Error;
         end case;
      end Actual_Ref;

      -----------------
      -- AREC_String --
      -----------------

      function AREC_String (Lev : Pos) return String is
      begin
         if Lev > 9 then
            return AREC_String (Lev / 10) & Character'Val (Lev mod 10 + 48);
         else
            return "AREC" & Character'Val (Lev + 48);
         end if;
      end AREC_String;

      --------------------
      -- Enclosing_Subp --
      --------------------

      function Enclosing_Subp (Subp : SI_Type) return SI_Type is
         STJ : Subp_Entry renames Subps.Table (Subp);
         Ret : constant SI_Type := Subp_Index (Enclosing_Subprogram (STJ.Ent));
      begin
         pragma Assert (STJ.Lev > 1);
         pragma Assert (Subps.Table (Ret).Lev = STJ.Lev - 1);
         return Ret;
      end Enclosing_Subp;

      ---------------
      -- Get_Level --
      ---------------

      function Get_Level (Sub : Entity_Id) return Nat is
         Lev : Nat;
         S   : Entity_Id;

      begin
         Lev := 1;
         S   := Sub;
         loop
            if S = Subp then
               return Lev;
            else
               S := Enclosing_Subprogram (S);
               Lev := Lev + 1;
            end if;
         end loop;
      end Get_Level;

      ----------------
      -- Subp_Index --
      ----------------

      function Subp_Index (Sub : Entity_Id) return SI_Type is
      begin
         pragma Assert (Is_Subprogram (Sub));
         return SI_Type (UI_To_Int (Subps_Index (Sub)));
      end Subp_Index;

   --  Start of processing for Unnest_Subprogram

   begin
      --  Nothing to do inside a generic (all processing is for instance)

      if Inside_A_Generic then
         return;
      end if;
      --  At least for now, do not unnest anything but main source unit

      if not In_Extended_Main_Source_Unit (Subp_Body) then
         return;
      end if;

      --  First step, we must mark all nested subprograms that require a static
      --  link (activation record) because either they contain explicit uplevel
      --  references (as indicated by Has_Uplevel_Reference being set at this
      --  point), or they make calls to other subprograms in the same nest that
      --  require a static link (in which case we set this flag).

      --  This is a recursive definition, and to implement this, we have to
      --  build a call graph for the set of nested subprograms, and then go
      --  over this graph to implement recursively the invariant that if a
      --  subprogram has a call to a subprogram requiring a static link, then
      --  the calling subprogram requires a static link.

      --  First populate the above tables

      Subps.Init;
      Calls.Init;

      Build_Tables : declare
         function Visit_Node (N : Node_Id) return Traverse_Result;
         --  Visit a single node in Subp

         ----------------
         -- Visit_Node --
         ----------------

         function Visit_Node (N : Node_Id) return Traverse_Result is
            Ent  : Entity_Id;
            Csub : Entity_Id;

            function Find_Current_Subprogram return Entity_Id;
            --  Finds the current subprogram containing the call N

            -----------------------------
            -- Find_Current_Subprogram --
            -----------------------------

            function Find_Current_Subprogram return Entity_Id is
               Nod : Node_Id;

            begin
               Nod := N;
               loop
                  Nod := Parent (Nod);

                  if Nkind (Nod) = N_Subprogram_Body then
                     if Acts_As_Spec (Nod) then
                        return Defining_Entity (Specification (Nod));
                     else
                        return Corresponding_Spec (Nod);
                     end if;
                  end if;
               end loop;
            end Find_Current_Subprogram;

         --  Start of processing for Visit_Node

         begin
            --  Record a call

            if Nkind_In (N, N_Procedure_Call_Statement, N_Function_Call)

              --  We are only interested in direct calls, not indirect calls
              --  (where Name (N) is an explicit dereference) at least for now!

              and then Nkind (Name (N)) in N_Has_Entity
            then
               Ent := Entity (Name (N));

               --  We are only interested in calls to subprograms nested
               --  within Subp. Calls to Subp itself or to subprograms that
               --  are outside the nested structure do not affect us.

               if Scope_Within (Ent, Subp) then

                  --  For now, ignore calls to generic instances. Seems to be
                  --  some problem there which we will investigate later ???

                  if Original_Location (Sloc (Ent)) /= Sloc (Ent)
                    or else Is_Generic_Instance (Ent)
                  then
                     null;

                  --  Ignore calls to imported routines

                  elsif Is_Imported (Ent) then
                     null;

                  --  Here we have a call to keep and analyze

                  else
                     Csub := Find_Current_Subprogram;

                     --  Both caller and callee must be subprograms (we ignore
                     --  generic subprograms).

                     if Is_Subprogram (Csub) and then Is_Subprogram (Ent) then
                        Calls.Append ((N, Find_Current_Subprogram, Ent));
                     end if;
                  end if;
               end if;

            --  Record a subprogram. We record a subprogram body that acts as
            --  a spec. Otherwise we record a subprogram declaration, providing
            --  that it has a corresponding body we can get hold of. The case
            --  of no corresponding body being available is ignored for now.

            elsif (Nkind (N) = N_Subprogram_Body and then Acts_As_Spec (N))
              or else (Nkind (N) = N_Subprogram_Declaration
                        and then Present (Corresponding_Body (N)))
            then
               Subps.Increment_Last;

               declare
                  STJ : Subp_Entry renames Subps.Table (Subps.Last);

               begin
                  --  Set fields of Subp_Entry for new subprogram

                  STJ.Ent := Defining_Entity (Specification (N));
                  STJ.Lev := Get_Level (STJ.Ent);

                  if Nkind (N) = N_Subprogram_Body then
                     STJ.Bod := N;
                  else
                     STJ.Bod :=
                       Parent (Declaration_Node (Corresponding_Body (N)));
                     pragma Assert (Nkind (STJ.Bod) = N_Subprogram_Body);
                  end if;

                  --  Capture Uplevel_References, and then set (uses the same
                  --  field), the Subps_Index value for this subprogram.

                  STJ.Urefs := Uplevel_References (STJ.Ent);
                  Set_Subps_Index (STJ.Ent, UI_From_Int (Int (Subps.Last)));
               end;
            end if;

            return OK;
         end Visit_Node;

         -----------
         -- Visit --
         -----------

         procedure Visit is new Traverse_Proc (Visit_Node);
         --  Used to traverse the body of Subp, populating the tables

      --  Start of processing for Build_Tables

      begin
         --  A special case, if the outer level subprogram has a separate spec
         --  then we won't catch it in the traversal of the body. But we do
         --  want to visit the declaration in this case!

         if not Acts_As_Spec (Subp_Body) then
            declare
               Dummy : Traverse_Result;
               Decl  : constant Node_Id :=
                 Parent (Declaration_Node (Corresponding_Spec (Subp_Body)));
               pragma Assert (Nkind (Decl) = N_Subprogram_Declaration);
            begin
               Dummy := Visit_Node (Decl);
            end;
         end if;

         --  Traverse the body to get the rest of the subprograms and calls

         Visit (Subp_Body);
      end Build_Tables;

      --  Second step is to do the transitive closure, if any subprogram has
      --  a call to a subprogram for which Has_Uplevel_Reference is set, then
      --  we set Has_Uplevel_Reference for the calling routine.

      Closure : declare
         Modified : Boolean;

      begin
         --  We use a simple minded algorithm as follows (obviously this can
         --  be done more efficiently, using one of the standard algorithms
         --  for efficient transitive closure computation, but this is simple
         --  and most likely fast enough that its speed does not matter).

         --  Repeatedly scan the list of calls. Any time we find a call from
         --  A to B, where A does not have Has_Uplevel_Reference, and B does
         --  have this flag set, then set the flag for A, and note that we
         --  have made a change by setting Modified True. We repeat this until
         --  we make a pass with no modifications.

         Outer : loop
            Modified := False;
            Inner : for J in Calls.First .. Calls.Last loop
               if not Has_Uplevel_Reference (Calls.Table (J).From)
                 and then Has_Uplevel_Reference (Calls.Table (J).To)
               then
                  Set_Has_Uplevel_Reference (Calls.Table (J).From);
                  Modified := True;
               end if;
            end loop Inner;

            exit Outer when not Modified;
         end loop Outer;
      end Closure;

      --  Next step, create the entities for code we will insert. We do this
      --  at the start so that all the entities are defined, regardless of the
      --  order in which we do the code insertions.

      Create_Entities : for J in Subps.First .. Subps.Last loop
         declare
            STJ : Subp_Entry renames Subps.Table (J);
            Loc : constant Source_Ptr := Sloc (STJ.Bod);
            ARS : constant String     := AREC_String (STJ.Lev);

         begin
            --  First we create the ARECnF entity for the additional formal
            --  for all subprograms requiring that an activation record pointer
            --  be passed. This is true of all subprograms that have uplevel
            --  references, and whose enclosing subprogram also has uplevel
            --  references.

            if Has_Uplevel_Reference (STJ.Ent)
              and then STJ.Ent /= Subp
              and then Has_Uplevel_Reference (Enclosing_Subprogram (STJ.Ent))
            then
               STJ.ARECnF :=
                 Make_Defining_Identifier (Loc,
                   Chars => Name_Find_Str (AREC_String (STJ.Lev - 1) & "F"));
            else
               STJ.ARECnF := Empty;
            end if;

            --  Now define the AREC entities for the activation record. This
            --  is needed for any subprogram that has nested subprograms and
            --  has uplevel references.

            if Has_Nested_Subprogram (STJ.Ent)
              and then Has_Uplevel_Reference (STJ.Ent)
            then
               STJ.ARECn   :=
                 Make_Defining_Identifier (Loc, Name_Find_Str (ARS));
               STJ.ARECnT  :=
                 Make_Defining_Identifier (Loc, Name_Find_Str (ARS & "T"));
               STJ.ARECnPT :=
                 Make_Defining_Identifier (Loc, Name_Find_Str (ARS & "PT"));
               STJ.ARECnP  :=
                 Make_Defining_Identifier (Loc, Name_Find_Str (ARS & "P"));

            else
               STJ.ARECn   := Empty;
               STJ.ARECnT  := Empty;
               STJ.ARECnPT := Empty;
               STJ.ARECnP  := Empty;
               STJ.ARECnU  := Empty;
            end if;

            --  Define uplink component entity if inner nesting case

            if Has_Uplevel_Reference (STJ.Ent) and then STJ.Lev > 1 then
               declare
                  ARS1 : constant String := AREC_String (STJ.Lev - 1);
               begin
                  STJ.ARECnU :=
                    Make_Defining_Identifier (Loc,
                      Chars => Name_Find_Str (ARS1 & "U"));
               end;

            else
               STJ.ARECnU := Empty;
            end if;
         end;
      end loop Create_Entities;

      --  Loop through subprograms

      Subp_Loop : declare
         Addr : constant Entity_Id := RTE (RE_Address);

      begin
         for J in Subps.First .. Subps.Last loop
            declare
               STJ : Subp_Entry renames Subps.Table (J);

            begin
               --  First add the extra formal if needed. This applies to all
               --  nested subprograms that require an activation record to be
               --  passed, as indicated by ARECnF being defined.

               if Present (STJ.ARECnF) then

                  --  Here we need the extra formal. We do the expansion and
                  --  analysis of this manually, since it is fairly simple,
                  --  and it is not obvious how we can get what we want if we
                  --  try to use the normal Analyze circuit.

                  Add_Extra_Formal : declare
                     Encl : constant SI_Type := Enclosing_Subp (J);
                     STJE : Subp_Entry renames Subps.Table (Encl);
                     --  Index and Subp_Entry for enclosing routine

                     Form : constant Entity_Id := STJ.ARECnF;
                     --  The formal to be added. Note that n here is one less
                     --  than the level of the subprogram itself (STJ.Ent).

                     procedure Add_Form_To_Spec (F : Entity_Id; S : Node_Id);
                     --  S is an N_Function/Procedure_Specification node, and F
                     --  is the new entity to add to this subprogramn spec as
                     --  the last Extra_Formal.

                     ----------------------
                     -- Add_Form_To_Spec --
                     ----------------------

                     procedure Add_Form_To_Spec (F : Entity_Id; S : Node_Id) is
                        Sub : constant Entity_Id := Defining_Entity (S);
                        Ent : Entity_Id;

                     begin
                        --  Case of at least one Extra_Formal is present, set
                        --  ARECnF as the new last entry in the list.

                        if Present (Extra_Formals (Sub)) then
                           Ent := Extra_Formals (Sub);
                           while Present (Extra_Formal (Ent)) loop
                              Ent := Extra_Formal (Ent);
                           end loop;

                           Set_Extra_Formal (Ent, F);

                        --  No Extra formals present

                        else
                           Set_Extra_Formals (Sub, F);
                           Ent := Last_Formal (Sub);

                           if Present (Ent) then
                              Set_Extra_Formal (Ent, F);
                           end if;
                        end if;
                     end Add_Form_To_Spec;

                  --  Start of processing for Add_Extra_Formal

                  begin
                     --  Decorate the new formal entity

                     Set_Scope               (Form, STJ.Ent);
                     Set_Ekind               (Form, E_In_Parameter);
                     Set_Etype               (Form, STJE.ARECnPT);
                     Set_Mechanism           (Form, By_Copy);
                     Set_Never_Set_In_Source (Form, True);
                     Set_Analyzed            (Form, True);
                     Set_Comes_From_Source   (Form, False);

                     --  Case of only body present

                     if Acts_As_Spec (STJ.Bod) then
                        Add_Form_To_Spec (Form, Specification (STJ.Bod));

                     --  Case of separate spec

                     else
                        Add_Form_To_Spec (Form, Parent (STJ.Ent));
                     end if;
                  end Add_Extra_Formal;
               end if;

               --  Processing for subprograms that have at least one nested
               --  subprogram, and have uplevel references.

               if Has_Nested_Subprogram (STJ.Ent)
                 and then Has_Uplevel_Reference (STJ.Ent)
               then
                  --  Local declarations for one such subprogram

                  declare
                     Loc   : constant Source_Ptr := Sloc (STJ.Bod);
                     Elmt  : Elmt_Id;
                     Nod   : Node_Id;
                     Ent   : Entity_Id;
                     Clist : List_Id;
                     Comp  : Entity_Id;

                     Decl_ARECnT  : Node_Id;
                     Decl_ARECn   : Node_Id;
                     Decl_ARECnPT : Node_Id;
                     Decl_ARECnP  : Node_Id;
                     --  Declaration nodes for the AREC entities we build

                     Uplevel_Entities :
                       array (1 .. List_Length (STJ.Urefs)) of Entity_Id;
                     Num_Uplevel_Entities : Nat;
                     --  Uplevel_Entities (1 .. Num_Uplevel_Entities) contains
                     --  a list (with no duplicates) of the entities for this
                     --  subprogram that are referenced uplevel. The maximum
                     --  number of entries cannot exceed the total number of
                     --  uplevel references.

                  begin
                     --  Populate the Uplevel_Entities array, using the flag
                     --  Uplevel_Reference_Noted to avoid duplicates.

                     Num_Uplevel_Entities := 0;

                     if Present (STJ.Urefs) then
                        Elmt := First_Elmt (STJ.Urefs);
                        while Present (Elmt) loop
                           Nod := Actual_Ref (Node (Elmt));
                           Ent := Entity (Nod);

                           if not Uplevel_Reference_Noted (Ent) then
                              Set_Uplevel_Reference_Noted (Ent, True);
                              Num_Uplevel_Entities := Num_Uplevel_Entities + 1;
                              Uplevel_Entities (Num_Uplevel_Entities) := Ent;
                           end if;

                           Next_Elmt (Elmt);
                           Next_Elmt (Elmt);
                        end loop;
                     end if;

                     --  Build list of component declarations for ARECnT

                     Clist := Empty_List;

                     --  If we are in a subprogram that has a static link that
                     --  ias passed in (as indicated by ARECnF being deinfed),
                     --  then include ARECnU : ARECnPT := ARECnF where n is
                     --  one less than the current level and the entity ARECnPT
                     --  comes from the enclosing subprogram.

                     if Present (STJ.ARECnF) then
                        declare
                           STJE : Subp_Entry
                                    renames Subps.Table (Enclosing_Subp (J));

                        begin
                           Append_To (Clist,
                             Make_Component_Declaration (Loc,
                               Defining_Identifier  => STJ.ARECnU,
                               Component_Definition =>
                                 Make_Component_Definition (Loc,
                                   Subtype_Indication =>
                                     New_Occurrence_Of (STJE.ARECnPT, Loc)),
                               Expression           =>
                                 New_Occurrence_Of (STJ.ARECnF, Loc)));
                        end;
                     end if;

                     --  Add components for uplevel referenced entities

                     for J in 1 .. Num_Uplevel_Entities loop
                        Comp :=
                          Make_Defining_Identifier (Loc,
                            Chars => Chars (Uplevel_Entities (J)));

                        Set_Activation_Record_Component
                          (Uplevel_Entities (J), Comp);

                        Append_To (Clist,
                          Make_Component_Declaration (Loc,
                            Defining_Identifier  => Comp,
                            Component_Definition =>
                              Make_Component_Definition (Loc,
                                Subtype_Indication =>
                                  New_Occurrence_Of (Addr, Loc))));
                     end loop;

                     --  Now we can insert the AREC declarations into the body

                     --  type ARECnT is record .. end record;

                     Decl_ARECnT :=
                       Make_Full_Type_Declaration (Loc,
                         Defining_Identifier => STJ.ARECnT,
                         Type_Definition     =>
                           Make_Record_Definition (Loc,
                             Component_List =>
                               Make_Component_List (Loc,
                                 Component_Items => Clist)));

                     --  ARECn : aliased ARECnT;

                     Decl_ARECn :=
                       Make_Object_Declaration (Loc,
                         Defining_Identifier => STJ.ARECn,
                           Aliased_Present   => True,
                           Object_Definition =>
                             New_Occurrence_Of (STJ.ARECnT, Loc));

                     --  type ARECnPT is access all ARECnT;

                     Decl_ARECnPT :=
                       Make_Full_Type_Declaration (Loc,
                         Defining_Identifier => STJ.ARECnPT,
                         Type_Definition     =>
                           Make_Access_To_Object_Definition (Loc,
                             All_Present        => True,
                             Subtype_Indication =>
                               New_Occurrence_Of (STJ.ARECnT, Loc)));

                     --  ARECnP : constant ARECnPT := ARECn'Access;

                     Decl_ARECnP :=
                       Make_Object_Declaration (Loc,
                         Defining_Identifier => STJ.ARECnP,
                         Constant_Present    => True,
                         Object_Definition   =>
                           New_Occurrence_Of (STJ.ARECnPT, Loc),
                         Expression          =>
                           Make_Attribute_Reference (Loc,
                             Prefix           =>
                               New_Occurrence_Of (STJ.ARECn, Loc),
                             Attribute_Name => Name_Access));

                     Prepend_List_To (Declarations (STJ.Bod),
                       New_List
                         (Decl_ARECnT, Decl_ARECn, Decl_ARECnPT, Decl_ARECnP));

                     --  Analyze the newly inserted declarations. Note that we
                     --  do not need to establish the whole scope stack, since
                     --  we have already set all entity fields (so there will
                     --  be no searching of upper scopes to resolve names). But
                     --  we do set the scope of the current subprogram, so that
                     --  newly created entities go in the right entity chain.

                     --  We analyze with all checks suppressed (since we do
                     --  not expect any exceptions, and also we temporarily
                     --  turn off Unested_Subprogram_Mode to avoid trying to
                     --  mark uplevel references (not needed at this stage,
                     --  and in fact causes a bit of recursive chaos).

                     Push_Scope (STJ.Ent);
                     Opt.Unnest_Subprogram_Mode := False;
                     Analyze (Decl_ARECnT,  Suppress => All_Checks);
                     Analyze (Decl_ARECn,   Suppress => All_Checks);
                     Analyze (Decl_ARECnPT, Suppress => All_Checks);
                     Analyze (Decl_ARECnP,  Suppress => All_Checks);
                     Opt.Unnest_Subprogram_Mode := True;
                     Pop_Scope;

                     --  Next step, for each uplevel referenced entity, add
                     --  assignment operations to set the comoponent in the
                     --  activation record.

                     for J in 1 .. Num_Uplevel_Entities loop
                        declare
                           Ent : constant Entity_Id  := Uplevel_Entities (J);
                           Loc : constant Source_Ptr := Sloc (Ent);
                           Dec : constant Node_Id    := Declaration_Node (Ent);
                           Ins : Node_Id;
                           Asn : Node_Id;

                        begin
                           --  For parameters, we insert the assignment right
                           --  after the declaration of ARECnP. For all other
                           --  entities, we insert the assignment immediately
                           --  after the declaration of the entity.

                           --  Note: we don't need to mark the entity as being
                           --  aliased, because the address attribute will mark
                           --  it as Address_Taken, and that is good enough.

                           if Is_Formal (Ent) then
                              Ins := Decl_ARECnP;
                           else
                              Ins := Dec;
                           end if;

                           --  Build and insert the assignment:
                           --    ARECn.nam := nam

                           Asn :=
                             Make_Assignment_Statement (Loc,
                               Name       =>
                                 Make_Selected_Component (Loc,
                                   Prefix        =>
                                     New_Occurrence_Of (STJ.ARECn, Loc),
                                   Selector_Name =>
                                     Make_Identifier (Loc, Chars (Ent))),

                               Expression =>
                                 Make_Attribute_Reference (Loc,
                                   Prefix         =>
                                     New_Occurrence_Of (Ent, Loc),
                                   Attribute_Name => Name_Address));

                           Insert_After (Ins, Asn);

                           --  Analyze the assignment statement. We do not need
                           --  to establish the relevant scope stack entries
                           --  here, because we have already set the correct
                           --  entity references, so no name resolution is
                           --  required, and no new entities are created, so
                           --  we don't even need to set the current scope.

                           --  We analyze with all checks suppressed (since
                           --  we do not expect any exceptions, and also we
                           --  temporarily turn off Unested_Subprogram_Mode
                           --  to avoid trying to mark uplevel references (not
                           --  needed at this stage, and in fact causes a bit
                           --  of recursive chaos).

                           Opt.Unnest_Subprogram_Mode := False;
                           Analyze (Asn, Suppress => All_Checks);
                           Opt.Unnest_Subprogram_Mode := True;
                        end;
                     end loop;
                  end;
               end if;
            end;
         end loop;
      end Subp_Loop;

      --  Next step, process uplevel references. This has to be done in a
      --  separate pass, after completing the processing in Sub_Loop because we
      --  need all the AREC declarations generated, inserted, and analyzed so
      --  that the uplevel references can be successfully analyzed.

      Uplev_Refs : for J in Subps.First .. Subps.Last loop
         declare
            STJ : Subp_Entry renames Subps.Table (J);

         begin
            --  We are only interested in entries which have uplevel references
            --  to deal with, as indicated by the Urefs list being present

            if Present (STJ.Urefs) then

               --  Process uplevel references for one subprogram

               declare
                  Elmt : Elmt_Id;

               begin
                  --  Loop through uplevel references

                  Elmt := First_Elmt (STJ.Urefs);
                  while Present (Elmt) loop

                     --  Rewrite one reference

                     declare
                        Ref : constant Node_Id := Actual_Ref (Node (Elmt));
                        --  The reference to be rewritten

                        Loc : constant Source_Ptr := Sloc (Ref);
                        --  Source location for the reference

                        Ent : constant Entity_Id := Entity (Ref);
                        --  The referenced entity

                        Typ : constant Entity_Id := Etype (Ent);
                        --  The type of the referenced entity

                        Rsub : constant Entity_Id :=
                                 Node (Next_Elmt (Elmt));
                        --  The enclosing subprogram for the reference

                        RSX : constant SI_Type := Subp_Index (Rsub);
                        --  Subp_Index for enclosing subprogram for ref

                        STJR : Subp_Entry renames Subps.Table (RSX);
                        --  Subp_Entry for enclosing subprogram for ref

                        Tnn : constant Entity_Id :=
                                Make_Temporary
                                  (Loc, 'T', Related_Node => Ref);
                        --  Local pointer type for reference

                        Pfx  : Node_Id;
                        Comp : Entity_Id;
                        SI   : SI_Type;

                     begin
                        --  Push the current scope, so that the pointer type
                        --  Tnn, and any subsidiary entities resulting from
                        --  the analysis of the rewritten reference, go in the
                        --  right entity chain.

                        Push_Scope (STJR.Ent);

                        --  First insert declaration for pointer type

                        --    type Tnn is access all typ;

                        Insert_Action (Node (Elmt),
                          Make_Full_Type_Declaration (Loc,
                            Defining_Identifier => Tnn,
                            Type_Definition     =>
                              Make_Access_To_Object_Definition (Loc,
                                All_Present        => True,
                                Subtype_Indication =>
                                  New_Occurrence_Of (Typ, Loc))));

                        --  Now we need to rewrite the reference. We have a
                        --  reference is from level STJE.Lev to level STJ.Lev.
                        --  The general form of the rewritten reference for
                        --  entity X is:

                        --    Tnn!(ARECaF.ARECbU.ARECcU.ARECdU....ARECm.X).all

                        --  where a,b,c,d .. m =
                        --         STJR.Lev - 1,  STJ.Lev - 2, .. STJ.Lev

                        pragma Assert (STJR.Lev > STJ.Lev);

                        --  Compute the prefix of X. Here are examples to make
                        --  things clear (with parens to show groupings, the
                        --  prefix is everything except the .X at the end).

                        --   level 2 to level 1

                        --     AREC1F.X

                        --   level 3 to level 1

                        --     (AREC2F.AREC1U).X

                        --   level 4 to level 1

                        --     ((AREC3F.AREC2U).AREC1U).X

                        --   level 6 to level 2

                        --     (((AREC5F.AREC4U).AREC3U).AREC2U).X

                        Pfx := New_Occurrence_Of (STJR.ARECnF, Loc);
                        SI := RSX;
                        for L in STJ.Lev .. STJR.Lev - 2 loop
                           SI := Enclosing_Subp (SI);
                           Pfx :=
                             Make_Selected_Component (Loc,
                               Prefix        => Pfx,
                               Selector_Name =>
                                 New_Occurrence_Of
                                   (Subps.Table (SI).ARECnU, Loc));
                        end loop;

                        --  Get activation record component (must exist)

                        Comp := Activation_Record_Component (Ent);
                        pragma Assert (Present (Comp));

                        --  Do the replacement

                        Rewrite (Ref,
                          Make_Explicit_Dereference (Loc,
                            Prefix =>
                              Unchecked_Convert_To (Tnn,
                                Make_Selected_Component (Loc,
                                  Prefix        => Pfx,
                                  Selector_Name =>
                                    New_Occurrence_Of (Comp, Loc)))));

                        --  Analyze and resolve the new expression. We do not
                        --  need to establish the relevant scope stack entries
                        --  here, because we have already set all the correct
                        --  entity references, so no name resolution is needed.
                        --  We have already set the current scope, so that any
                        --  new entities created will be in the right scope.

                        --  We analyze with all checks suppressed (since we do
                        --  not expect any exceptions, and also we temporarily
                        --  turn off Unested_Subprogram_Mode to avoid trying to
                        --  mark uplevel references (not needed at this stage,
                        --  and in fact causes a bit of recursive chaos).

                        Opt.Unnest_Subprogram_Mode := False;
                        Analyze_And_Resolve (Ref, Typ, Suppress => All_Checks);
                        Opt.Unnest_Subprogram_Mode := True;
                        Pop_Scope;
                     end;

                     Next_Elmt (Elmt);
                     Next_Elmt (Elmt);
                  end loop;
               end;
            end if;
         end;
      end loop Uplev_Refs;

      --  Finally, loop through all calls adding extra actual for the
      --  activation record where it is required.

      Adjust_Calls : for J in Calls.First .. Calls.Last loop

         --  Process a single call, we are only interested in a call to a
         --  subprogram that actually needs a pointer to an activation record,
         --  as indicated by the ARECnF entity being set. This excludes the
         --  top level subprogram, and any subprogram not having uplevel refs.

         Adjust_One_Call : declare
            CTJ : Call_Entry renames Calls.Table (J);
            STF : Subp_Entry renames Subps.Table (Subp_Index (CTJ.From));
            STT : Subp_Entry renames Subps.Table (Subp_Index (CTJ.To));

            Loc : constant Source_Ptr := Sloc (CTJ.N);

            Extra  : Node_Id;
            ExtraP : Node_Id;
            SubX   : SI_Type;
            Act    : Node_Id;

         begin
            if Present (STT.ARECnF) then

               --  CTJ.N is a call to a subprogram which may require
               --  a pointer to an activation record. The subprogram
               --  containing the call is CTJ.From and the subprogram being
               --  called is CTJ.To, so we have a call from level STF.Lev to
               --  level STT.Lev.

               --  There are three possibilities:

               --  For a call to the same level, we just pass the activation
               --  record passed to the calling subprogram.

               if STF.Lev = STT.Lev then
                  Extra := New_Occurrence_Of (STF.ARECnF, Loc);

               --  For a call that goes down a level, we pass a pointer
               --  to the activation record constructed wtihin the caller
               --  (which may be the outer level subprogram, but also may
               --  be a more deeply nested caller).

               elsif STT.Lev = STF.Lev + 1 then
                  Extra := New_Occurrence_Of (STF.ARECnP, Loc);

                  --  Otherwise we must have an upcall (STT.Lev < STF.LEV),
                  --  since it is not possible to do a downcall of more than
                  --  one level.

                  --  For a call from level STF.Lev to level STT.Lev, we
                  --  have to find the activation record needed by the
                  --  callee. This is as follows:

                  --    ARECaF.ARECbU.ARECcU....ARECm

                  --  where a,b,c .. m =
                  --    STF.Lev - 1,  STF.Lev - 2, STF.Lev - 3 .. STT.Lev

               else
                  pragma Assert (STT.Lev < STF.Lev);

                  Extra := New_Occurrence_Of (STF.ARECnF, Loc);
                  SubX := Subp_Index (CTJ.From);
                  for K in reverse STT.Lev .. STF.Lev - 1 loop
                     SubX := Enclosing_Subp (SubX);
                     Extra :=
                       Make_Selected_Component (Loc,
                         Prefix        => Extra,
                         Selector_Name =>
                           New_Occurrence_Of
                             (Subps.Table (SubX).ARECnU, Loc));
                  end loop;
               end if;

               --  Extra is the additional parameter to be added. Build a
               --  parameter association that we can append to the actuals.

               ExtraP :=
                 Make_Parameter_Association (Loc,
                   Selector_Name             =>
                     New_Occurrence_Of (STT.ARECnF, Loc),
                   Explicit_Actual_Parameter => Extra);

               if No (Parameter_Associations (CTJ.N)) then
                  Set_Parameter_Associations (CTJ.N, Empty_List);
               end if;

               Append (ExtraP, Parameter_Associations (CTJ.N));

               --  We need to deal with the actual parameter chain as well.
               --  The newly added parameter is always the last actual.

               Act := First_Named_Actual (CTJ.N);

               if No (Act) then
                  Set_First_Named_Actual (CTJ.N, Extra);

               --  Here we must follow the chain and append the new entry

               else
                  loop
                     declare
                        PAN : Node_Id;
                        NNA : Node_Id;

                     begin
                        PAN := Parent (Act);
                        pragma Assert (Nkind (PAN) = N_Parameter_Association);
                        NNA := Next_Named_Actual (PAN);

                        if No (NNA) then
                           Set_Next_Named_Actual (PAN, Extra);
                           exit;
                        end if;

                        Act := NNA;
                     end;
                  end loop;
               end if;

               --  Analyze and resolve the new actual. We do not need to
               --  establish the relevant scope stack entries here, because
               --  we have already set all the correct entity references, so
               --  no name resolution is needed.

               --  We analyze with all checks suppressed (since we do not
               --  expect any exceptions, and also we temporarily turn off
               --  Unested_Subprogram_Mode to avoid trying to mark uplevel
               --  references (not needed at this stage, and in fact causes
               --  a bit of recursive chaos).

               Opt.Unnest_Subprogram_Mode := False;
               Analyze_And_Resolve
                 (Extra, Etype (STT.ARECnF), Suppress => All_Checks);
               Opt.Unnest_Subprogram_Mode := True;
            end if;
         end Adjust_One_Call;
      end loop Adjust_Calls;

      return;
   end Unnest_Subprogram;

end Exp_Unst;
