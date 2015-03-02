------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ U N S T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2015, Free Software Foundation, Inc.            --
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
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Rtsfind;  use Rtsfind;
with Sem_Aux;  use Sem_Aux;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Table;
with Tbuild;   use Tbuild;

package body Exp_Unst is

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
                  while Present (T) loop
                     if Check_Dynamic_Type (C) then
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
      --  Nothing to do if we know this is a static type

      if Is_Static_Type (Typ) then
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
   begin
      --  Establish list if first call for Uplevel_References

      if No (Uplevel_References (Subp)) then
         Set_Uplevel_References (Subp, New_Elmt_List);
      end if;

      --  Add new element to Uplevel_References

      Append_Elmt (N, Uplevel_References (Subp));
      Set_Has_Uplevel_Reference (Entity (N));
   end Note_Uplevel_Reference;

   -----------------------
   -- Unnest_Subprogram --
   -----------------------

   --  Tables used by Unnest_Subprogram

   type Subp_Entry is record
      Ent : Entity_Id;
      --  Entity of the subprogram

      Bod : Node_Id;
      --  Subprogram_Body node for this subprogram

      Lev : Nat;
      --  Subprogram level (1 = outer subprogram (Subp argument), 2 = nested
      --  immediately within this outer subprogram etc.)
   end record;

   package Subps is new Table.Table (
     Table_Component_Type => Subp_Entry,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => 100,
     Table_Increment      => 200,
     Table_Name           => "Subps");
   --  Records the subprograms in the nest whose outer subprogram is Subp

   type Call_Entry is record
      N   : Node_Id;
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
     Table_Name           => "Calls");
   --  Records each call within the outer subprogram and all nested subprograms
   --  that are to other subprograms nested within the outer subprogram. These
   --  are the calls that may need an additional parameter.

   procedure Unnest_Subprogram (Subp : Entity_Id; Subp_Body : Node_Id) is

      function Get_AREC_String (Lev : Pos) return String;
      --  Given a level value, 1, 2, ... returns the string AREC, AREC2, ...

      function Get_Level (Sub : Entity_Id) return Nat;
      --  Sub is either Subp itself, or a subprogram nested within Subp. This
      --  function returns the level of nesting (Subp = 1, subprograms that
      --  are immediately nested within Subp = 2, etc).

      ---------------------
      -- Get_AREC_String --
      ---------------------

      function Get_AREC_String (Lev : Pos) return String is
      begin
         if Lev > 9 then
            return
              Get_AREC_String (Lev / 10) & Character'Val (Lev mod 10 + 48);
         else
            return
              "AREC" & Character'Val (Lev + 48);
         end if;
      end Get_AREC_String;

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
               S := Enclosing_Dynamic_Scope (S);
               Lev := Lev + 1;
            end if;
         end loop;
      end Get_Level;

   --  Start of processing for Unnest_Subprogram

   begin
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

      --  First step, populate the above tables

      Subps.Init;
      Calls.Init;

      Build_Tables : declare
         function Visit_Node (N : Node_Id) return Traverse_Result;
         --  Visit a single node in Subp

         ----------------
         -- Visit_Node --
         ----------------

         function Visit_Node (N : Node_Id) return Traverse_Result is
            Ent : Entity_Id;

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
                        return Defining_Unit_Name (Specification (Nod));
                     else
                        return Corresponding_Spec (Nod);
                     end if;
                  end if;
               end loop;
            end Find_Current_Subprogram;

         --  Start of processing for Visit_Node

         begin
            if Nkind_In (N, N_Procedure_Call_Statement, N_Function_Call) then
               Ent := Entity (Name (N));

               if not Is_Library_Level_Entity (Ent) then
                  Calls.Append ((N, Find_Current_Subprogram, Ent));
               end if;

            elsif Nkind (N) = N_Subprogram_Body and then Acts_As_Spec (N) then
               Ent := Defining_Unit_Name (Specification (N));
               Subps.Append
                 ((Ent => Ent,
                   Bod => N,
                   Lev => Get_Level (Ent)));

            elsif Nkind (N) = N_Subprogram_Declaration then
               Ent := Defining_Unit_Name (Specification (N));
               Subps.Append
                 ((Ent => Ent,
                   Bod => Corresponding_Body (N),
                   Lev => Get_Level (Ent)));
            end if;

            return OK;
         end Visit_Node;

         -----------
         -- Visit --
         -----------

         procedure Visit is new Traverse_Proc (Visit_Node);
         --  Used to traverse the body of Subp, populating the tables

      begin
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

      --  Next step, process each subprogram in turn, inserting necessary
      --  declarations for ARECxx types and variables for any subprogram
      --  that has nested subprograms, and is uplevel referenced.

      Arec_Decls : declare
         Addr : constant Entity_Id := RTE (RE_Address);

      begin
         for J in Subps.First .. Subps.Last loop
            declare
               STJ : Subp_Entry renames Subps.Table (J);

            begin
               --  We add AREC declarations for any subprogram that has at
               --  least one nested subprogram, and has uplevel references.

               if Has_Nested_Subprogram (STJ.Ent)
                 and then Has_Uplevel_Reference (STJ.Ent)
               then
                  Add_AREC_Declarations : declare
                     Loc   : constant Source_Ptr := Sloc (STJ.Bod);
                     ARS   : constant String     := Get_AREC_String (STJ.Lev);
                     Urefs : constant Elist_Id   :=
                               Uplevel_References (STJ.Ent);
                     Elmt  : Elmt_Id;
                     Ent   : Entity_Id;
                     Clist : List_Id;

                     Uplevel_Entities :
                       array (1 .. List_Length (Urefs)) of Entity_Id;
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
                     Elmt := First_Elmt (Urefs);
                     while Present (Elmt) loop
                        Ent := Entity (Node (Elmt));

                        if not Uplevel_Reference_Noted (Ent) then
                           Set_Uplevel_Reference_Noted (Ent, True);
                           Num_Uplevel_Entities := Num_Uplevel_Entities + 1;
                           Uplevel_Entities (Num_Uplevel_Entities) := Ent;
                        end if;

                        Next_Elmt (Elmt);
                     end loop;

                     --  Build list of component declarations for ARECnT

                     Clist := Empty_List;

                     --  If not top level, include ARECn : ARECnPT := ARECnP

                     if STJ.Lev > 1 then
                        Append_To (Clist,
                          Make_Component_Declaration (Loc,
                            Defining_Identifier =>
                              Make_Defining_Identifier (Loc,
                                Chars => Name_Find_Str (ARS)),
                            Component_Definition =>
                              Make_Component_Definition (Loc,
                                Subtype_Indication =>
                                  Make_Identifier (Loc,
                                    Chars => Name_Find_Str (ARS & "PT"))),
                            Expression =>
                              Make_Identifier (Loc,
                                Chars => Name_Find_Str (ARS & "P"))));
                     end if;

                     --  Add components for uplevel referenced entities

                     for J in 1 .. Num_Uplevel_Entities loop
                        Append_To (Clist,
                          Make_Component_Declaration (Loc,
                            Defining_Identifier =>
                              Make_Defining_Identifier (Loc,
                                Chars => Chars (Uplevel_Entities (J))),
                            Component_Definition =>
                              Make_Component_Definition (Loc,
                                Subtype_Indication =>
                                  New_Occurrence_Of (Addr, Loc))));
                     end loop;

                     --  Now we can insert the AREC declarations into the body

                     Prepend_List_To (Declarations (STJ.Bod),
                       New_List (

                         --  type ARECT is record .. end record;

                         Make_Full_Type_Declaration (Loc,
                           Defining_Identifier =>
                             Make_Defining_Identifier (Loc,
                               Chars => Name_Find_Str (ARS & "T")),
                           Type_Definition     =>
                             Make_Record_Definition (Loc,
                               Component_List =>
                                 Make_Component_List (Loc,
                                   Component_Items => Clist))),

                         --  type ARECPT is access all ARECT;

                         Make_Full_Type_Declaration (Loc,
                           Defining_Identifier =>
                             Make_Defining_Identifier (Loc,
                               Chars => Name_Find_Str (ARS & "PT")),
                             Type_Definition   =>
                                Make_Access_To_Object_Definition (Loc,
                                  All_Present        => True,
                                  Subtype_Indication =>
                                    Make_Identifier (Loc,
                                      Chars => Name_Find_Str (ARS & "T")))),

                        --  ARECP : constant ARECPT := AREC'Access;

                        Make_Object_Declaration (Loc,
                          Defining_Identifier =>
                            Make_Defining_Identifier (Loc,
                              Chars => Name_Find_Str (ARS & "P")),
                          Constant_Present    => True,
                          Object_Definition   =>
                            Make_Identifier (Loc, Name_Find_Str (ARS & "PT")),
                          Expression          =>
                            Make_Attribute_Reference (Loc,
                              Prefix         =>
                                Make_Identifier (Loc, Name_Find_Str (ARS)),
                                  Attribute_Name => Name_Access))));
                  end Add_AREC_Declarations;
               end if;
            end;
         end loop;
      end Arec_Decls;

      --  Next step, for each uplevel referenced entity, add assignment
      --  operations to set the corresponding AREC fields, and define
      --  the PTR types.

      return;
   end Unnest_Subprogram;

end Exp_Unst;
