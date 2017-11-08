------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--              L I B . X R E F . S P A R K _ S P E C I F I C               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2011-2017, Free Software Foundation, Inc.         --
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

with Einfo;       use Einfo;
with Nmake;       use Nmake;
with SPARK_Xrefs; use SPARK_Xrefs;

with GNAT.HTable;

separate (Lib.Xref)
package body SPARK_Specific is

   ---------------------
   -- Local Constants --
   ---------------------

   --  Table of SPARK_Entities, True for each entity kind used in SPARK

   SPARK_Entities : constant array (Entity_Kind) of Boolean :=
     (E_Constant         => True,
      E_Entry            => True,
      E_Function         => True,
      E_In_Out_Parameter => True,
      E_In_Parameter     => True,
      E_Loop_Parameter   => True,
      E_Operator         => True,
      E_Out_Parameter    => True,
      E_Procedure        => True,
      E_Variable         => True,
      others             => False);

   --  True for each reference type used in SPARK

   SPARK_References : constant array (Character) of Boolean :=
     ('m'    => True,
      'r'    => True,
      's'    => True,
      others => False);

   type Entity_Hashed_Range is range 0 .. 255;
   --  Size of hash table headers

   ---------------------
   -- Local Variables --
   ---------------------

   package Drefs is new Table.Table (
     Table_Component_Type => Xref_Entry,
     Table_Index_Type     => Xref_Entry_Number,
     Table_Low_Bound      => 1,
     Table_Initial        => Alloc.Drefs_Initial,
     Table_Increment      => Alloc.Drefs_Increment,
     Table_Name           => "Drefs");
   --  Table of cross-references for reads and writes through explicit
   --  dereferences, that are output as reads/writes to the special variable
   --  "Heap". These references are added to the regular references when
   --  computing SPARK cross-references.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Add_SPARK_File (Uspec, Ubody : Unit_Number_Type; Dspec : Nat);
   --  Add file and corresponding scopes for unit to the tables
   --  SPARK_File_Table and SPARK_Scope_Table. When two units are present
   --  for the same compilation unit, as it happens for library-level
   --  instantiations of generics, then Ubody is the number of the body
   --  unit; otherwise it is No_Unit.

   procedure Add_SPARK_Xrefs;
   --  Filter table Xrefs to add all references used in SPARK to the table
   --  SPARK_Xref_Table.

   function Entity_Hash (E : Entity_Id) return Entity_Hashed_Range;
   --  Hash function for hash table

   --------------------
   -- Add_SPARK_File --
   --------------------

   procedure Add_SPARK_File (Uspec, Ubody : Unit_Number_Type; Dspec : Nat) is
      File : constant Source_File_Index := Source_Index (Uspec);
      From : constant Scope_Index       := SPARK_Scope_Table.Last + 1;

      Scope_Id : Pos := 1;

      procedure Add_SPARK_Scope (N : Node_Id);
      --  Add scope N to the table SPARK_Scope_Table

      procedure Detect_And_Add_SPARK_Scope (N : Node_Id);
      --  Call Add_SPARK_Scope on scopes

      ---------------------
      -- Add_SPARK_Scope --
      ---------------------

      procedure Add_SPARK_Scope (N : Node_Id) is
         E : constant Entity_Id := Defining_Entity (N);

      begin
         --  Ignore scopes without a proper location

         if Sloc (N) = No_Location then
            return;
         end if;

         case Ekind (E) is
            when E_Entry
               | E_Entry_Family
               | E_Function
               | E_Generic_Function
               | E_Generic_Package
               | E_Generic_Procedure
               | E_Package
               | E_Package_Body
               | E_Procedure
               | E_Protected_Body
               | E_Protected_Type
               | E_Task_Body
               | E_Task_Type
               | E_Subprogram_Body
            =>
               null;

            when E_Void =>

               --  Compilation of prj-attr.adb with -gnatn creates a node with
               --  entity E_Void for the package defined at a-charac.ads16:13.
               --  ??? TBD

               return;

            when others =>
               raise Program_Error;
         end case;

         --  File_Num and Scope_Num are filled later. From_Xref and To_Xref
         --  are filled even later, but are initialized to represent an empty
         --  range.

         SPARK_Scope_Table.Append
           ((Scope_Id       => E,
             File_Num       => Dspec,
             Scope_Num      => Scope_Id,
             Spec_File_Num  => 0,
             Spec_Scope_Num => 0,
             From_Xref      => 1,
             To_Xref        => 0));

         Scope_Id := Scope_Id + 1;
      end Add_SPARK_Scope;

      --------------------------------
      -- Detect_And_Add_SPARK_Scope --
      --------------------------------

      procedure Detect_And_Add_SPARK_Scope (N : Node_Id) is
      begin
         --  Entries

         if Nkind_In (N, N_Entry_Body, N_Entry_Declaration)

           --  Packages

           or else Nkind_In (N, N_Package_Body,
                                N_Package_Declaration)
           --  Protected units

           or else Nkind_In (N, N_Protected_Body,
                                N_Protected_Type_Declaration)

           --  Subprograms

           or else Nkind_In (N, N_Subprogram_Body,
                                N_Subprogram_Declaration)

           --  Task units

           or else Nkind_In (N, N_Task_Body,
                                N_Task_Type_Declaration)
         then
            Add_SPARK_Scope (N);
         end if;
      end Detect_And_Add_SPARK_Scope;

      procedure Traverse_Scopes is new
        Traverse_Compilation_Unit (Detect_And_Add_SPARK_Scope);

      --  Local variables

      File_Name      : String_Ptr;
      Unit_File_Name : String_Ptr;

   --  Start of processing for Add_SPARK_File

   begin
      --  Source file could be inexistant as a result of an error, if option
      --  gnatQ is used.

      if File <= No_Source_File then
         return;
      end if;

      --  Subunits are traversed as part of the top-level unit to which they
      --  belong.

      if Nkind (Unit (Cunit (Uspec))) = N_Subunit then
         return;
      end if;

      Traverse_Scopes (CU => Cunit (Uspec), Inside_Stubs => True);

      --  When two units are present for the same compilation unit, as it
      --  happens for library-level instantiations of generics, then add all
      --  scopes to the same SPARK file.

      if Ubody /= No_Unit then
         Traverse_Scopes (CU => Cunit (Ubody), Inside_Stubs => True);
      end if;

      --  Make entry for new file in file table

      Get_Name_String (Reference_Name (File));
      File_Name := new String'(Name_Buffer (1 .. Name_Len));

      --  For subunits, also retrieve the file name of the unit. Only do so if
      --  unit has an associated compilation unit.

      if Present (Cunit (Unit (File)))
        and then Nkind (Unit (Cunit (Unit (File)))) = N_Subunit
      then
         Get_Name_String (Reference_Name (Main_Source_File));
         Unit_File_Name := new String'(Name_Buffer (1 .. Name_Len));
      else
         Unit_File_Name := null;
      end if;

      SPARK_File_Table.Append (
        (File_Name      => File_Name,
         Unit_File_Name => Unit_File_Name,
         File_Num       => Dspec,
         From_Scope     => From,
         To_Scope       => SPARK_Scope_Table.Last));
   end Add_SPARK_File;

   ---------------------
   -- Add_SPARK_Xrefs --
   ---------------------

   procedure Add_SPARK_Xrefs is
      function Entity_Of_Scope (S : Scope_Index) return Entity_Id;
      --  Return the entity which maps to the input scope index

      function Get_Scope_Num (E : Entity_Id) return Nat;
      --  Return the scope number associated with the entity E

      function Is_Constant_Object_Without_Variable_Input
        (E : Entity_Id) return Boolean;
      --  Return True if E is known to have no variable input, as defined in
      --  SPARK RM.

      function Is_Future_Scope_Entity
        (E : Entity_Id;
         S : Scope_Index) return Boolean;
      --  Check whether entity E is in SPARK_Scope_Table at index S or higher

      function Is_SPARK_Reference
        (E   : Entity_Id;
         Typ : Character) return Boolean;
      --  Return whether entity reference E meets SPARK requirements. Typ is
      --  the reference type.

      function Is_SPARK_Scope (E : Entity_Id) return Boolean;
      --  Return whether the entity or reference scope meets requirements for
      --  being a SPARK scope.

      function Lt (Op1 : Natural; Op2 : Natural) return Boolean;
      --  Comparison function for Sort call

      procedure Move (From : Natural; To : Natural);
      --  Move procedure for Sort call

      procedure Set_Scope_Num (E : Entity_Id; Num : Nat);
      --  Associate entity E with the scope number Num

      procedure Update_Scope_Range
        (S    : Scope_Index;
         From : Xref_Index;
         To   : Xref_Index);
      --  Update the scope which maps to S with the new range From .. To

      package Sorting is new GNAT.Heap_Sort_G (Move, Lt);

      No_Scope : constant Nat := 0;
      --  Initial scope counter

      package Scopes is new GNAT.HTable.Simple_HTable
        (Header_Num => Entity_Hashed_Range,
         Element    => Nat,
         No_Element => No_Scope,
         Key        => Entity_Id,
         Hash       => Entity_Hash,
         Equal      => "=");
      --  Package used to build a correspondence between entities and scope
      --  numbers used in SPARK cross references.

      Nrefs : Nat := Xrefs.Last;
      --  Number of references in table. This value may get reset (reduced)
      --  when we eliminate duplicate reference entries as well as references
      --  not suitable for local cross-references.

      Nrefs_Add : constant Nat := Drefs.Last;
      --  Number of additional references which correspond to dereferences in
      --  the source code.

      Rnums : array (0 .. Nrefs + Nrefs_Add) of Nat;
      --  This array contains numbers of references in the Xrefs table. This
      --  list is sorted in output order. The extra 0'th entry is convenient
      --  for the call to sort. When we sort the table, we move the indices in
      --  Rnums around, but we do not move the original table entries.

      ---------------------
      -- Entity_Of_Scope --
      ---------------------

      function Entity_Of_Scope (S : Scope_Index) return Entity_Id is
      begin
         return SPARK_Scope_Table.Table (S).Scope_Id;
      end Entity_Of_Scope;

      -------------------
      -- Get_Scope_Num --
      -------------------

      function Get_Scope_Num (E : Entity_Id) return Nat renames Scopes.Get;

      -----------------------------------------------
      -- Is_Constant_Object_Without_Variable_Input --
      -----------------------------------------------

      function Is_Constant_Object_Without_Variable_Input
        (E : Entity_Id) return Boolean
      is
      begin
         case Ekind (E) is

            --  A constant is known to have no variable input if its
            --  initializing expression is static (a value which is
            --  compile-time-known is not guaranteed to have no variable input
            --  as defined in the SPARK RM). Otherwise, the constant may or not
            --  have variable input.

            when E_Constant =>
               declare
                  Decl : Node_Id;
               begin
                  if Present (Full_View (E)) then
                     Decl := Parent (Full_View (E));
                  else
                     Decl := Parent (E);
                  end if;

                  if Is_Imported (E) then
                     return False;
                  else
                     pragma Assert (Present (Expression (Decl)));
                     return Is_Static_Expression (Expression (Decl));
                  end if;
               end;

            when E_In_Parameter
               | E_Loop_Parameter
            =>
               return True;

            when others =>
               return False;
         end case;
      end Is_Constant_Object_Without_Variable_Input;

      ----------------------------
      -- Is_Future_Scope_Entity --
      ----------------------------

      function Is_Future_Scope_Entity
        (E : Entity_Id;
         S : Scope_Index) return Boolean
      is
         function Is_Past_Scope_Entity return Boolean;
         --  Check whether entity E is in SPARK_Scope_Table at index strictly
         --  lower than S.

         --------------------------
         -- Is_Past_Scope_Entity --
         --------------------------

         function Is_Past_Scope_Entity return Boolean is
         begin
            for Index in SPARK_Scope_Table.First .. S - 1 loop
               if SPARK_Scope_Table.Table (Index).Scope_Id = E then
                  return True;
               end if;
            end loop;

            return False;
         end Is_Past_Scope_Entity;

      --  Start of processing for Is_Future_Scope_Entity

      begin
         for Index in S .. SPARK_Scope_Table.Last loop
            if SPARK_Scope_Table.Table (Index).Scope_Id = E then
               return True;
            end if;
         end loop;

         --  If this assertion fails, this means that the scope which we are
         --  looking for has been treated already, which reveals a problem in
         --  the order of cross-references.

         pragma Assert (not Is_Past_Scope_Entity);

         return False;
      end Is_Future_Scope_Entity;

      ------------------------
      -- Is_SPARK_Reference --
      ------------------------

      function Is_SPARK_Reference
        (E   : Entity_Id;
         Typ : Character) return Boolean
      is
      begin
         --  The only references of interest on callable entities are calls. On
         --  uncallable entities, the only references of interest are reads and
         --  writes.

         if Ekind (E) in Overloadable_Kind then
            return Typ = 's';

         --  In all other cases, result is true for reference/modify cases,
         --  and false for all other cases.

         else
            return Typ = 'r' or else Typ = 'm';
         end if;
      end Is_SPARK_Reference;

      --------------------
      -- Is_SPARK_Scope --
      --------------------

      function Is_SPARK_Scope (E : Entity_Id) return Boolean is
         Can_Be_Renamed : constant Boolean :=
                            Present (E)
                              and then (Is_Subprogram_Or_Entry (E)
                                         or else Ekind (E) = E_Package);
      begin
         return Present (E)
           and then not Is_Generic_Unit (E)
           and then (not Can_Be_Renamed or else No (Renamed_Entity (E)))
           and then Get_Scope_Num (E) /= No_Scope;
      end Is_SPARK_Scope;

      --------
      -- Lt --
      --------

      function Lt (Op1 : Natural; Op2 : Natural) return Boolean is
         T1 : constant Xref_Entry := Xrefs.Table (Rnums (Nat (Op1)));
         T2 : constant Xref_Entry := Xrefs.Table (Rnums (Nat (Op2)));

      begin
         --  First test: if entity is in different unit, sort by unit. Note:
         --  that we use Ent_Scope_File rather than Eun, as Eun may refer to
         --  the file where the generic scope is defined, which may differ from
         --  the file where the enclosing scope is defined. It is the latter
         --  which matters for a correct order here.

         if T1.Ent_Scope_File /= T2.Ent_Scope_File then
            return Dependency_Num (T1.Ent_Scope_File) <
                   Dependency_Num (T2.Ent_Scope_File);

         --  Second test: within same unit, sort by location of the scope of
         --  the entity definition.

         elsif Get_Scope_Num (T1.Key.Ent_Scope) /=
               Get_Scope_Num (T2.Key.Ent_Scope)
         then
            return Get_Scope_Num (T1.Key.Ent_Scope) <
                   Get_Scope_Num (T2.Key.Ent_Scope);

         --  Third test: within same unit and scope, sort by location of
         --  entity definition.

         elsif T1.Def /= T2.Def then
            return T1.Def < T2.Def;

         else
            --  Both entities must be equal at this point

            pragma Assert (T1.Key.Ent = T2.Key.Ent);
            pragma Assert (T1.Key.Ent_Scope = T2.Key.Ent_Scope);
            pragma Assert (T1.Ent_Scope_File = T2.Ent_Scope_File);

            --  Fourth test: if reference is in same unit as entity definition,
            --  sort first.

            if T1.Key.Lun /= T2.Key.Lun
              and then T1.Ent_Scope_File = T1.Key.Lun
            then
               return True;

            elsif T1.Key.Lun /= T2.Key.Lun
              and then T2.Ent_Scope_File = T2.Key.Lun
            then
               return False;

            --  Fifth test: if reference is in same unit and same scope as
            --  entity definition, sort first.

            elsif T1.Ent_Scope_File = T1.Key.Lun
              and then T1.Key.Ref_Scope /= T2.Key.Ref_Scope
              and then T1.Key.Ent_Scope = T1.Key.Ref_Scope
            then
               return True;

            elsif T2.Ent_Scope_File = T2.Key.Lun
              and then T1.Key.Ref_Scope /= T2.Key.Ref_Scope
              and then T2.Key.Ent_Scope = T2.Key.Ref_Scope
            then
               return False;

            --  Sixth test: for same entity, sort by reference location unit

            elsif T1.Key.Lun /= T2.Key.Lun then
               return Dependency_Num (T1.Key.Lun) <
                      Dependency_Num (T2.Key.Lun);

            --  Seventh test: for same entity, sort by reference location scope

            elsif Get_Scope_Num (T1.Key.Ref_Scope) /=
                  Get_Scope_Num (T2.Key.Ref_Scope)
            then
               return Get_Scope_Num (T1.Key.Ref_Scope) <
                      Get_Scope_Num (T2.Key.Ref_Scope);

            --  Eighth test: order of location within referencing unit

            elsif T1.Key.Loc /= T2.Key.Loc then
               return T1.Key.Loc < T2.Key.Loc;

            --  Finally, for two locations at the same address prefer the one
            --  that does NOT have the type 'r', so that a modification or
            --  extension takes preference, when there are more than one
            --  reference at the same location. As a result, in the case of
            --  entities that are in-out actuals, the read reference follows
            --  the modify reference.

            else
               return T2.Key.Typ = 'r';
            end if;
         end if;
      end Lt;

      ----------
      -- Move --
      ----------

      procedure Move (From : Natural; To : Natural) is
      begin
         Rnums (Nat (To)) := Rnums (Nat (From));
      end Move;

      -------------------
      -- Set_Scope_Num --
      -------------------

      procedure Set_Scope_Num (E : Entity_Id; Num : Nat) renames Scopes.Set;

      ------------------------
      -- Update_Scope_Range --
      ------------------------

      procedure Update_Scope_Range
        (S    : Scope_Index;
         From : Xref_Index;
         To   : Xref_Index)
      is
      begin
         SPARK_Scope_Table.Table (S).From_Xref := From;
         SPARK_Scope_Table.Table (S).To_Xref := To;
      end Update_Scope_Range;

      --  Local variables

      From_Index : Xref_Index;
      Prev_Loc   : Source_Ptr;
      Prev_Typ   : Character;
      Ref_Count  : Nat;
      Scope_Id   : Scope_Index;

   --  Start of processing for Add_SPARK_Xrefs

   begin
      for Index in SPARK_Scope_Table.First .. SPARK_Scope_Table.Last loop
         declare
            S : SPARK_Scope_Record renames SPARK_Scope_Table.Table (Index);
         begin
            Set_Scope_Num (S.Scope_Id, S.Scope_Num);
         end;
      end loop;

      declare
         Drefs_Table : Drefs.Table_Type
                         renames Drefs.Table (Drefs.First .. Drefs.Last);
      begin
         Xrefs.Append_All (Xrefs.Table_Type (Drefs_Table));
         Nrefs := Nrefs + Drefs_Table'Length;
      end;

      --  Capture the definition Sloc values. As in the case of normal cross
      --  references, we have to wait until now to get the correct value.

      for Index in 1 .. Nrefs loop
         Xrefs.Table (Index).Def := Sloc (Xrefs.Table (Index).Key.Ent);
      end loop;

      --  Eliminate entries not appropriate for SPARK. Done prior to sorting
      --  cross-references, as it discards useless references which do not have
      --  a proper format for the comparison function (like no location).

      Ref_Count := Nrefs;
      Nrefs     := 0;

      for Index in 1 .. Ref_Count loop
         declare
            Ref : Xref_Key renames Xrefs.Table (Index).Key;

         begin
            if SPARK_Entities (Ekind (Ref.Ent))
              and then SPARK_References (Ref.Typ)
              and then Is_SPARK_Scope (Ref.Ent_Scope)
              and then Is_SPARK_Scope (Ref.Ref_Scope)
              and then Is_SPARK_Reference (Ref.Ent, Ref.Typ)

              --  Discard references from unknown scopes, e.g. generic scopes

              and then Get_Scope_Num (Ref.Ent_Scope) /= No_Scope
              and then Get_Scope_Num (Ref.Ref_Scope) /= No_Scope

              --  Discard references to loop parameters introduced within
              --  expression functions, as they give two references: one from
              --  the analysis of the expression function itself and one from
              --  the analysis of the expanded body. We don't lose any globals
              --  by discarding them, because such loop parameters can only be
              --  accessed locally from within the expression function body.

              and then not
                (Ekind (Ref.Ent) = E_Loop_Parameter
                  and then Scope_Within
                             (Ref.Ent, Unique_Entity (Ref.Ref_Scope))
                  and then Is_Expression_Function (Ref.Ref_Scope))
            then
               Nrefs         := Nrefs + 1;
               Rnums (Nrefs) := Index;
            end if;
         end;
      end loop;

      --  Sort the references

      Sorting.Sort (Integer (Nrefs));

      --  Eliminate duplicate entries

      --  We need this test for Ref_Count because if we force ALI file
      --  generation in case of errors detected, it may be the case that
      --  Nrefs is 0, so we should not reset it here.

      if Nrefs >= 2 then
         Ref_Count := Nrefs;
         Nrefs     := 1;

         for Index in 2 .. Ref_Count loop
            if Xrefs.Table (Rnums (Index)) /= Xrefs.Table (Rnums (Nrefs)) then
               Nrefs := Nrefs + 1;
               Rnums (Nrefs) := Rnums (Index);
            end if;
         end loop;
      end if;

      --  Eliminate the reference if it is at the same location as the previous
      --  one, unless it is a read-reference indicating that the entity is an
      --  in-out actual in a call.

      Ref_Count := Nrefs;
      Nrefs     := 0;
      Prev_Loc  := No_Location;
      Prev_Typ  := 'm';

      for Index in 1 .. Ref_Count loop
         declare
            Ref : Xref_Key renames Xrefs.Table (Rnums (Index)).Key;

         begin
            if Ref.Loc /= Prev_Loc
              or else (Prev_Typ = 'm' and then Ref.Typ = 'r')
            then
               Prev_Loc      := Ref.Loc;
               Prev_Typ      := Ref.Typ;
               Nrefs         := Nrefs + 1;
               Rnums (Nrefs) := Rnums (Index);
            end if;
         end;
      end loop;

      --  The two steps have eliminated all references, nothing to do

      if SPARK_Scope_Table.Last = 0 then
         return;
      end if;

      Scope_Id   := 1;
      From_Index := 1;

      --  Loop to output references

      for Refno in 1 .. Nrefs loop
         declare
            Ref_Entry : Xref_Entry renames Xrefs.Table (Rnums (Refno));
            Ref       : Xref_Key   renames Ref_Entry.Key;
            Typ       : Character;

         begin
            --  If this assertion fails, the scope which we are looking for is
            --  not in SPARK scope table, which reveals either a problem in the
            --  construction of the scope table, or an erroneous scope for the
            --  current cross-reference.

            pragma Assert (Is_Future_Scope_Entity (Ref.Ent_Scope, Scope_Id));

            --  Update the range of cross references to which the current scope
            --  refers to. This may be the empty range only for the first scope
            --  considered.

            if Ref.Ent_Scope /= Entity_Of_Scope (Scope_Id) then
               Update_Scope_Range
                 (S    => Scope_Id,
                  From => From_Index,
                  To   => SPARK_Xref_Table.Last);

               From_Index := SPARK_Xref_Table.Last + 1;
            end if;

            while Ref.Ent_Scope /= Entity_Of_Scope (Scope_Id) loop
               Scope_Id := Scope_Id + 1;
               pragma Assert (Scope_Id <= SPARK_Scope_Table.Last);
            end loop;

            --  References to constant objects without variable inputs (see
            --  SPARK RM 3.3.1) are considered specially in SPARK section,
            --  because these will be translated as constants in the
            --  intermediate language for formal verification, and should
            --  therefore never appear in frame conditions. Other constants may
            --  later be treated the same, up to GNATprove to decide based on
            --  its flow analysis.

            if Is_Constant_Object_Without_Variable_Input (Ref.Ent) then
               Typ := 'c';
            else
               Typ := Ref.Typ;
            end if;

            SPARK_Xref_Table.Append (
              (Entity    => Unique_Entity (Ref.Ent),
               File_Num  => Dependency_Num (Ref.Lun),
               Scope_Num => Get_Scope_Num (Ref.Ref_Scope),
               Rtype     => Typ));
         end;
      end loop;

      --  Update the range of cross references to which the scope refers to

      Update_Scope_Range
        (S    => Scope_Id,
         From => From_Index,
         To   => SPARK_Xref_Table.Last);
   end Add_SPARK_Xrefs;

   -------------------------
   -- Collect_SPARK_Xrefs --
   -------------------------

   procedure Collect_SPARK_Xrefs
     (Sdep_Table : Unit_Ref_Table;
      Num_Sdep   : Nat)
   is
      Sdep      : Pos;
      Sdep_Next : Pos;
      --  Index of the current and next source dependency

      Sdep_File : Pos;
      --  Index of the file to which the scopes need to be assigned; for
      --  library-level instances of generic units this points to the unit
      --  of the body, because this is where references are assigned to.

      Ubody : Unit_Number_Type;
      Uspec : Unit_Number_Type;
      --  Unit numbers for the dependency spec and possibly its body (only in
      --  the case of library-level instance of a generic package).

   begin
      --  Cross-references should have been computed first

      pragma Assert (Xrefs.Last /= 0);

      Initialize_SPARK_Tables;

      --  Generate file and scope SPARK cross-reference information

      Sdep := 1;
      while Sdep <= Num_Sdep loop

         --  Skip dependencies with no entity node, e.g. configuration files
         --  with pragmas (.adc) or target description (.atp), since they
         --  present no interest for SPARK cross references.

         if No (Cunit_Entity (Sdep_Table (Sdep))) then
            Sdep_Next := Sdep + 1;

         --  For library-level instantiation of a generic, two consecutive
         --  units refer to the same compilation unit node and entity (one to
         --  body, one to spec). In that case, treat them as a single unit for
         --  the sake of SPARK cross references by passing to Add_SPARK_File.

         else
            if Sdep < Num_Sdep
              and then Cunit_Entity (Sdep_Table (Sdep)) =
                       Cunit_Entity (Sdep_Table (Sdep + 1))
            then
               declare
                  Cunit1 : Node_Id renames Cunit (Sdep_Table (Sdep));
                  Cunit2 : Node_Id renames Cunit (Sdep_Table (Sdep + 1));

               begin
                  --  Both Cunits point to compilation unit nodes

                  pragma Assert
                    (Nkind (Cunit1) = N_Compilation_Unit
                      and then Nkind (Cunit2) = N_Compilation_Unit);

                  --  Do not depend on the sorting order, which is based on
                  --  Unit_Name, and for library-level instances of nested
                  --  generic packages they are equal.

                  --  If declaration comes before the body

                  if Nkind (Unit (Cunit1)) = N_Package_Declaration
                    and then Nkind (Unit (Cunit2)) = N_Package_Body
                  then
                     Uspec := Sdep_Table (Sdep);
                     Ubody := Sdep_Table (Sdep + 1);

                     Sdep_File := Sdep + 1;

                  --  If body comes before declaration

                  elsif Nkind (Unit (Cunit1)) = N_Package_Body
                    and then Nkind (Unit (Cunit2)) = N_Package_Declaration
                  then
                     Uspec := Sdep_Table (Sdep + 1);
                     Ubody := Sdep_Table (Sdep);

                     Sdep_File := Sdep;

                  --  Otherwise it is an error

                  else
                     raise Program_Error;
                  end if;

                  Sdep_Next := Sdep + 2;
               end;

            --  ??? otherwise?

            else
               Uspec := Sdep_Table (Sdep);
               Ubody := No_Unit;

               Sdep_File := Sdep;
               Sdep_Next := Sdep + 1;
            end if;

            Add_SPARK_File
              (Uspec => Uspec,
               Ubody => Ubody,
               Dspec => Sdep_File);
         end if;

         Sdep := Sdep_Next;
      end loop;

      --  Fill in the spec information when relevant

      declare
         package Entity_Hash_Table is new
           GNAT.HTable.Simple_HTable
             (Header_Num => Entity_Hashed_Range,
              Element    => Scope_Index,
              No_Element => 0,
              Key        => Entity_Id,
              Hash       => Entity_Hash,
              Equal      => "=");

      begin
         --  Fill in the hash-table

         for S in SPARK_Scope_Table.First .. SPARK_Scope_Table.Last loop
            declare
               Srec : SPARK_Scope_Record renames SPARK_Scope_Table.Table (S);
            begin
               Entity_Hash_Table.Set (Srec.Scope_Id, S);
            end;
         end loop;

         --  Use the hash-table to locate spec entities

         for S in SPARK_Scope_Table.First .. SPARK_Scope_Table.Last loop
            declare
               Srec : SPARK_Scope_Record renames SPARK_Scope_Table.Table (S);

               Spec_Entity : constant Entity_Id :=
                               Unique_Entity (Srec.Scope_Id);
               Spec_Scope  : constant Scope_Index :=
                               Entity_Hash_Table.Get (Spec_Entity);

            begin
               --  Generic spec may be missing in which case Spec_Scope is zero

               if Spec_Entity /= Srec.Scope_Id
                 and then Spec_Scope /= 0
               then
                  Srec.Spec_File_Num :=
                    SPARK_Scope_Table.Table (Spec_Scope).File_Num;
                  Srec.Spec_Scope_Num :=
                    SPARK_Scope_Table.Table (Spec_Scope).Scope_Num;
               end if;
            end;
         end loop;
      end;

      --  Generate SPARK cross-reference information

      Add_SPARK_Xrefs;
   end Collect_SPARK_Xrefs;

   -------------------------------------
   -- Enclosing_Subprogram_Or_Package --
   -------------------------------------

   function Enclosing_Subprogram_Or_Library_Package
     (N : Node_Id) return Entity_Id
   is
      Context : Entity_Id;

   begin
      --  If N is the defining identifier for a subprogram, then return the
      --  enclosing subprogram or package, not this subprogram.

      if Nkind_In (N, N_Defining_Identifier, N_Defining_Operator_Symbol)
        and then (Ekind (N) in Entry_Kind
                   or else Ekind (N) = E_Subprogram_Body
                   or else Ekind (N) in Generic_Subprogram_Kind
                   or else Ekind (N) in Subprogram_Kind)
      then
         Context := Parent (Unit_Declaration_Node (N));

         --  If this was a library-level subprogram then replace Context with
         --  its Unit, which points to N_Subprogram_* node.

         if Nkind (Context) = N_Compilation_Unit then
            Context := Unit (Context);
         end if;
      else
         Context := N;
      end if;

      while Present (Context) loop
         case Nkind (Context) is
            when N_Package_Body
               | N_Package_Specification
            =>
               --  Only return a library-level package

               if Is_Library_Level_Entity (Defining_Entity (Context)) then
                  Context := Defining_Entity (Context);
                  exit;
               else
                  Context := Parent (Context);
               end if;

            when N_Pragma =>

               --  The enclosing subprogram for a precondition, postcondition,
               --  or contract case should be the declaration preceding the
               --  pragma (skipping any other pragmas between this pragma and
               --  this declaration.

               while Nkind (Context) = N_Pragma
                 and then Is_List_Member (Context)
                 and then Present (Prev (Context))
               loop
                  Context := Prev (Context);
               end loop;

               if Nkind (Context) = N_Pragma then
                  Context := Parent (Context);
               end if;

            when N_Entry_Body
               | N_Entry_Declaration
               | N_Protected_Type_Declaration
               | N_Subprogram_Body
               | N_Subprogram_Declaration
               | N_Subprogram_Specification
               | N_Task_Body
               | N_Task_Type_Declaration
            =>
               Context := Defining_Entity (Context);
               exit;

            when others =>
               Context := Parent (Context);
         end case;
      end loop;

      if Nkind (Context) = N_Defining_Program_Unit_Name then
         Context := Defining_Identifier (Context);
      end if;

      --  Do not return a scope without a proper location

      if Present (Context)
        and then Sloc (Context) = No_Location
      then
         return Empty;
      end if;

      return Context;
   end Enclosing_Subprogram_Or_Library_Package;

   -----------------
   -- Entity_Hash --
   -----------------

   function Entity_Hash (E : Entity_Id) return Entity_Hashed_Range is
   begin
      return
        Entity_Hashed_Range (E mod (Entity_Id (Entity_Hashed_Range'Last) + 1));
   end Entity_Hash;

   --------------------------
   -- Generate_Dereference --
   --------------------------

   procedure Generate_Dereference
     (N   : Node_Id;
      Typ : Character := 'r')
   is
      procedure Create_Heap;
      --  Create and decorate the special entity which denotes the heap

      -----------------
      -- Create_Heap --
      -----------------

      procedure Create_Heap is
      begin
         Name_Len := Name_Of_Heap_Variable'Length;
         Name_Buffer (1 .. Name_Len) := Name_Of_Heap_Variable;

         Heap := Make_Defining_Identifier (Standard_Location, Name_Enter);

         Set_Ekind       (Heap, E_Variable);
         Set_Is_Internal (Heap, True);
         Set_Has_Fully_Qualified_Name (Heap);
      end Create_Heap;

      --  Local variables

      Loc : constant Source_Ptr := Sloc (N);

   --  Start of processing for Generate_Dereference

   begin
      if Loc > No_Location then
         Drefs.Increment_Last;

         declare
            Deref_Entry : Xref_Entry renames Drefs.Table (Drefs.Last);
            Deref       : Xref_Key   renames Deref_Entry.Key;

         begin
            if No (Heap) then
               Create_Heap;
            end if;

            Deref.Ent := Heap;
            Deref.Loc := Loc;
            Deref.Typ := Typ;

            --  It is as if the special "Heap" was defined in the main unit,
            --  in the scope of the entity for the main unit. This single
            --  definition point is required to ensure that sorting cross
            --  references works for "Heap" references as well.

            Deref.Eun := Main_Unit;
            Deref.Lun := Get_Top_Level_Code_Unit (Loc);

            Deref.Ref_Scope := Enclosing_Subprogram_Or_Library_Package (N);
            Deref.Ent_Scope := Cunit_Entity (Main_Unit);

            Deref_Entry.Def := No_Location;

            Deref_Entry.Ent_Scope_File := Main_Unit;
         end;
      end if;
   end Generate_Dereference;

   -------------------------------
   -- Traverse_Compilation_Unit --
   -------------------------------

   procedure Traverse_Compilation_Unit
     (CU           : Node_Id;
      Inside_Stubs : Boolean)
   is
      procedure Traverse_Block                      (N : Node_Id);
      procedure Traverse_Declaration_Or_Statement   (N : Node_Id);
      procedure Traverse_Declarations_And_HSS       (N : Node_Id);
      procedure Traverse_Declarations_Or_Statements (L : List_Id);
      procedure Traverse_Handled_Statement_Sequence (N : Node_Id);
      procedure Traverse_Package_Body               (N : Node_Id);
      procedure Traverse_Visible_And_Private_Parts  (N : Node_Id);
      procedure Traverse_Protected_Body             (N : Node_Id);
      procedure Traverse_Subprogram_Body            (N : Node_Id);
      procedure Traverse_Task_Body                  (N : Node_Id);

      --  Traverse corresponding construct, calling Process on all declarations

      --------------------
      -- Traverse_Block --
      --------------------

      procedure Traverse_Block (N : Node_Id) renames
        Traverse_Declarations_And_HSS;

      ---------------------------------------
      -- Traverse_Declaration_Or_Statement --
      ---------------------------------------

      procedure Traverse_Declaration_Or_Statement (N : Node_Id) is
         function Traverse_Stub (N : Node_Id) return Boolean;
         --  Returns True iff stub N should be traversed

         function Traverse_Stub (N : Node_Id) return Boolean is
         begin
            pragma Assert (Nkind_In (N, N_Package_Body_Stub,
                                        N_Protected_Body_Stub,
                                        N_Subprogram_Body_Stub,
                                        N_Task_Body_Stub));

            return Inside_Stubs and then Present (Library_Unit (N));
         end Traverse_Stub;

      --  Start of processing for Traverse_Declaration_Or_Statement

      begin
         case Nkind (N) is
            when N_Package_Declaration =>
               Traverse_Visible_And_Private_Parts (Specification (N));

            when N_Package_Body =>
               Traverse_Package_Body (N);

            when N_Package_Body_Stub =>
               if Traverse_Stub (N) then
                  Traverse_Package_Body (Get_Body_From_Stub (N));
               end if;

            when N_Subprogram_Body =>
               Traverse_Subprogram_Body (N);

            when N_Entry_Body =>
               Traverse_Subprogram_Body (N);

            when N_Subprogram_Body_Stub =>
               if Traverse_Stub (N) then
                  Traverse_Subprogram_Body (Get_Body_From_Stub (N));
               end if;

            when N_Protected_Body =>
               Traverse_Protected_Body (N);

            when N_Protected_Body_Stub =>
               if Traverse_Stub (N) then
                  Traverse_Protected_Body (Get_Body_From_Stub (N));
               end if;

            when N_Protected_Type_Declaration =>
               Traverse_Visible_And_Private_Parts (Protected_Definition (N));

            when N_Task_Type_Declaration =>

               --  Task type definition is optional (unlike protected type
               --  definition, which is mandatory).

               declare
                  Task_Def : constant Node_Id := Task_Definition (N);
               begin
                  if Present (Task_Def) then
                     Traverse_Visible_And_Private_Parts (Task_Def);
                  end if;
               end;

            when N_Task_Body =>
               Traverse_Task_Body (N);

            when N_Task_Body_Stub =>
               if Traverse_Stub (N) then
                  Traverse_Task_Body (Get_Body_From_Stub (N));
               end if;

            when N_Block_Statement =>
               Traverse_Block (N);

            when N_If_Statement =>

               --  Traverse the statements in the THEN part

               Traverse_Declarations_Or_Statements (Then_Statements (N));

               --  Loop through ELSIF parts if present

               if Present (Elsif_Parts (N)) then
                  declare
                     Elif : Node_Id := First (Elsif_Parts (N));

                  begin
                     while Present (Elif) loop
                        Traverse_Declarations_Or_Statements
                          (Then_Statements (Elif));
                        Next (Elif);
                     end loop;
                  end;
               end if;

               --  Finally traverse the ELSE statements if present

               Traverse_Declarations_Or_Statements (Else_Statements (N));

            when N_Case_Statement =>

               --  Process case branches

               declare
                  Alt : Node_Id := First (Alternatives (N));
               begin
                  loop
                     Traverse_Declarations_Or_Statements (Statements (Alt));
                     Next (Alt);
                     exit when No (Alt);
                  end loop;
               end;

            when N_Extended_Return_Statement =>
               Traverse_Handled_Statement_Sequence
                 (Handled_Statement_Sequence (N));

            when N_Loop_Statement =>
               Traverse_Declarations_Or_Statements (Statements (N));

               --  Generic declarations are ignored

            when others =>
               null;
         end case;
      end Traverse_Declaration_Or_Statement;

      -----------------------------------
      -- Traverse_Declarations_And_HSS --
      -----------------------------------

      procedure Traverse_Declarations_And_HSS (N : Node_Id) is
      begin
         Traverse_Declarations_Or_Statements (Declarations (N));
         Traverse_Handled_Statement_Sequence (Handled_Statement_Sequence (N));
      end Traverse_Declarations_And_HSS;

      -----------------------------------------
      -- Traverse_Declarations_Or_Statements --
      -----------------------------------------

      procedure Traverse_Declarations_Or_Statements (L : List_Id) is
         N : Node_Id;

      begin
         --  Loop through statements or declarations

         N := First (L);
         while Present (N) loop

            --  Call Process on all declarations

            if Nkind (N) in N_Declaration
              or else Nkind (N) in N_Later_Decl_Item
              or else Nkind (N) = N_Entry_Body
            then
               if Nkind (N) in N_Body_Stub then
                  Process (Get_Body_From_Stub (N));
               else
                  Process (N);
               end if;
            end if;

            Traverse_Declaration_Or_Statement (N);

            Next (N);
         end loop;
      end Traverse_Declarations_Or_Statements;

      -----------------------------------------
      -- Traverse_Handled_Statement_Sequence --
      -----------------------------------------

      procedure Traverse_Handled_Statement_Sequence (N : Node_Id) is
         Handler : Node_Id;

      begin
         if Present (N) then
            Traverse_Declarations_Or_Statements (Statements (N));

            if Present (Exception_Handlers (N)) then
               Handler := First (Exception_Handlers (N));
               while Present (Handler) loop
                  Traverse_Declarations_Or_Statements (Statements (Handler));
                  Next (Handler);
               end loop;
            end if;
         end if;
      end Traverse_Handled_Statement_Sequence;

      ---------------------------
      -- Traverse_Package_Body --
      ---------------------------

      procedure Traverse_Package_Body (N : Node_Id) is
         Spec_E : constant Entity_Id := Unique_Defining_Entity (N);

      begin
         case Ekind (Spec_E) is
            when E_Package =>
               Traverse_Declarations_And_HSS (N);

            when E_Generic_Package =>
               null;

            when others =>
               raise Program_Error;
         end case;
      end Traverse_Package_Body;

      -----------------------------
      -- Traverse_Protected_Body --
      -----------------------------

      procedure Traverse_Protected_Body (N : Node_Id) is
      begin
         Traverse_Declarations_Or_Statements (Declarations (N));
      end Traverse_Protected_Body;

      ------------------------------
      -- Traverse_Subprogram_Body --
      ------------------------------

      procedure Traverse_Subprogram_Body (N : Node_Id) is
         Spec_E : constant Entity_Id := Unique_Defining_Entity (N);

      begin
         case Ekind (Spec_E) is
            when Entry_Kind
               | E_Function
               | E_Procedure
            =>
               Traverse_Declarations_And_HSS (N);

            when Generic_Subprogram_Kind =>
               null;

            when others =>
               raise Program_Error;
         end case;
      end Traverse_Subprogram_Body;

      ------------------------
      -- Traverse_Task_Body --
      ------------------------

      procedure Traverse_Task_Body (N : Node_Id) renames
        Traverse_Declarations_And_HSS;

      ----------------------------------------
      -- Traverse_Visible_And_Private_Parts --
      ----------------------------------------

      procedure Traverse_Visible_And_Private_Parts (N : Node_Id) is
      begin
         Traverse_Declarations_Or_Statements (Visible_Declarations (N));
         Traverse_Declarations_Or_Statements (Private_Declarations (N));
      end Traverse_Visible_And_Private_Parts;

      --  Local variables

      Lu : Node_Id;

   --  Start of processing for Traverse_Compilation_Unit

   begin
      --  Get Unit (checking case of subunit)

      Lu := Unit (CU);

      if Nkind (Lu) = N_Subunit then
         Lu := Proper_Body (Lu);
      end if;

      --  Do not add scopes for generic units

      if Nkind (Lu) = N_Package_Body
        and then Ekind (Corresponding_Spec (Lu)) in Generic_Unit_Kind
      then
         return;
      end if;

      --  Call Process on all declarations

      if Nkind (Lu) in N_Declaration
        or else Nkind (Lu) in N_Later_Decl_Item
      then
         Process (Lu);
      end if;

      --  Traverse the unit

      Traverse_Declaration_Or_Statement (Lu);
   end Traverse_Compilation_Unit;

end SPARK_Specific;
