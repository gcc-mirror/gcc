------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--              L I B . X R E F . S P A R K _ S P E C I F I C               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2011-2013, Free Software Foundation, Inc.         --
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

with SPARK_Xrefs;     use SPARK_Xrefs;
with Einfo;           use Einfo;
with Nmake;           use Nmake;
with Put_SPARK_Xrefs;

with GNAT.HTable;

separate (Lib.Xref)
package body SPARK_Specific is

   ---------------------
   -- Local Constants --
   ---------------------

   --  Table of SPARK_Entities, True for each entity kind used in SPARK

   SPARK_Entities : constant array (Entity_Kind) of Boolean :=
     (E_Constant         => True,
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
     ('m' => True,
      'r' => True,
      's' => True,
      others => False);

   type Entity_Hashed_Range is range 0 .. 255;
   --  Size of hash table headers

   ---------------------
   -- Local Variables --
   ---------------------

   Heap : Entity_Id := Empty;
   --  A special entity which denotes the heap object

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

   procedure Add_SPARK_File (Ubody, Uspec : Unit_Number_Type; Dspec : Nat);
   --  Add file and corresponding scopes for unit to the tables
   --  SPARK_File_Table and SPARK_Scope_Table. When two units are present for
   --  the same compilation unit, as it happens for library-level
   --  instantiations of generics, then Ubody /= Uspec, and all scopes are
   --  added to the same SPARK file. Otherwise Ubody = Uspec.

   procedure Add_SPARK_Scope (N : Node_Id);
   --  Add scope N to the table SPARK_Scope_Table

   procedure Add_SPARK_Xrefs;
   --  Filter table Xrefs to add all references used in SPARK to the table
   --  SPARK_Xref_Table.

   procedure Detect_And_Add_SPARK_Scope (N : Node_Id);
   --  Call Add_SPARK_Scope on scopes

   function Entity_Hash (E : Entity_Id) return Entity_Hashed_Range;
   --  Hash function for hash table

   procedure Traverse_Declarations_Or_Statements
     (L            : List_Id;
      Process      : Node_Processing;
      Inside_Stubs : Boolean);
   procedure Traverse_Handled_Statement_Sequence
     (N            : Node_Id;
      Process      : Node_Processing;
      Inside_Stubs : Boolean);
   procedure Traverse_Package_Body
     (N            : Node_Id;
      Process      : Node_Processing;
      Inside_Stubs : Boolean);
   procedure Traverse_Package_Declaration
     (N            : Node_Id;
      Process      : Node_Processing;
      Inside_Stubs : Boolean);
   procedure Traverse_Subprogram_Body
     (N            : Node_Id;
      Process      : Node_Processing;
      Inside_Stubs : Boolean);
   --  Traverse corresponding construct, calling Process on all declarations

   --------------------
   -- Add_SPARK_File --
   --------------------

   procedure Add_SPARK_File (Ubody, Uspec : Unit_Number_Type; Dspec : Nat) is
      File : constant Source_File_Index := Source_Index (Uspec);
      From : Scope_Index;

      File_Name      : String_Ptr;
      Unit_File_Name : String_Ptr;

   begin
      --  Source file could be inexistant as a result of an error, if option
      --  gnatQ is used.

      if File = No_Source_File then
         return;
      end if;

      From := SPARK_Scope_Table.Last + 1;

      --  Unit might not have an associated compilation unit, as seen in code
      --  filling Sdep_Table in Write_ALI.

      if Present (Cunit (Ubody)) then
         Traverse_Compilation_Unit
           (CU           => Cunit (Ubody),
            Process      => Detect_And_Add_SPARK_Scope'Access,
            Inside_Stubs => False);
      end if;

      --  When two units are present for the same compilation unit, as it
      --  happens for library-level instantiations of generics, then add all
      --  scopes to the same SPARK file.

      if Ubody /= Uspec then
         if Present (Cunit (Uspec)) then
            Traverse_Compilation_Unit
              (CU           => Cunit (Uspec),
               Process      => Detect_And_Add_SPARK_Scope'Access,
               Inside_Stubs => False);
         end if;
      end if;

      --  Update scope numbers

      declare
         Scope_Id : Int;
      begin
         Scope_Id := 1;
         for Index in From .. SPARK_Scope_Table.Last loop
            declare
               S : SPARK_Scope_Record renames SPARK_Scope_Table.Table (Index);
            begin
               S.Scope_Num := Scope_Id;
               S.File_Num  := Dspec;
               Scope_Id    := Scope_Id + 1;
            end;
         end loop;
      end;

      --  Remove those scopes previously marked for removal

      declare
         Scope_Id : Scope_Index;

      begin
         Scope_Id := From;
         for Index in From .. SPARK_Scope_Table.Last loop
            declare
               S : SPARK_Scope_Record renames SPARK_Scope_Table.Table (Index);
            begin
               if S.Scope_Num /= 0 then
                  SPARK_Scope_Table.Table (Scope_Id) := S;
                  Scope_Id := Scope_Id + 1;
               end if;
            end;
         end loop;

         SPARK_Scope_Table.Set_Last (Scope_Id - 1);
      end;

      --  Make entry for new file in file table

      Get_Name_String (Reference_Name (File));
      File_Name := new String'(Name_Buffer (1 .. Name_Len));

      --  For subunits, also retrieve the file name of the unit. Only do so if
      --  unit has an associated compilation unit.

      if Present (Cunit (Uspec))
        and then Present (Cunit (Unit (File)))
        and then Nkind (Unit (Cunit (Unit (File)))) = N_Subunit
      then
         Get_Name_String (Reference_Name (Main_Source_File));
         Unit_File_Name := new String'(Name_Buffer (1 .. Name_Len));
      end if;

      SPARK_File_Table.Append (
        (File_Name      => File_Name,
         Unit_File_Name => Unit_File_Name,
         File_Num       => Dspec,
         From_Scope     => From,
         To_Scope       => SPARK_Scope_Table.Last));
   end Add_SPARK_File;

   ---------------------
   -- Add_SPARK_Scope --
   ---------------------

   procedure Add_SPARK_Scope (N : Node_Id) is
      E   : constant Entity_Id  := Defining_Entity (N);
      Loc : constant Source_Ptr := Sloc (E);
      Typ : Character;

   begin
      --  Ignore scopes without a proper location

      if Sloc (N) = No_Location then
         return;
      end if;

      case Ekind (E) is
         when E_Function | E_Generic_Function =>
            Typ := 'V';

         when E_Procedure | E_Generic_Procedure =>
            Typ := 'U';

         when E_Subprogram_Body =>
            declare
               Spec : Node_Id;

            begin
               Spec := Parent (E);

               if Nkind (Spec) = N_Defining_Program_Unit_Name then
                  Spec := Parent (Spec);
               end if;

               if Nkind (Spec) = N_Function_Specification then
                  Typ := 'V';
               else
                  pragma Assert
                    (Nkind (Spec) = N_Procedure_Specification);
                  Typ := 'U';
               end if;
            end;

         when E_Package | E_Package_Body | E_Generic_Package =>
            Typ := 'K';

         when E_Void =>
            --  Compilation of prj-attr.adb with -gnatn creates a node with
            --  entity E_Void for the package defined at a-charac.ads16:13

            --  ??? TBD

            return;

         when others =>
            raise Program_Error;
      end case;

      --  File_Num and Scope_Num are filled later. From_Xref and To_Xref are
      --  filled even later, but are initialized to represent an empty range.

      SPARK_Scope_Table.Append (
        (Scope_Name     => new String'(Unique_Name (E)),
         File_Num       => 0,
         Scope_Num      => 0,
         Spec_File_Num  => 0,
         Spec_Scope_Num => 0,
         Line           => Nat (Get_Logical_Line_Number (Loc)),
         Stype          => Typ,
         Col            => Nat (Get_Column_Number (Loc)),
         From_Xref      => 1,
         To_Xref        => 0,
         Scope_Entity   => E));
   end Add_SPARK_Scope;

   ---------------------
   -- Add_SPARK_Xrefs --
   ---------------------

   procedure Add_SPARK_Xrefs is
      function Entity_Of_Scope (S : Scope_Index) return Entity_Id;
      --  Return the entity which maps to the input scope index

      function Get_Entity_Type (E : Entity_Id) return Character;
      --  Return a character representing the type of entity

      function Is_SPARK_Reference
        (E   : Entity_Id;
         Typ : Character) return Boolean;
      --  Return whether entity reference E meets SPARK requirements. Typ is
      --  the reference type.

      function Is_SPARK_Scope (E : Entity_Id) return Boolean;
      --  Return whether the entity or reference scope meets requirements for
      --  being an SPARK scope.

      function Is_Future_Scope_Entity
        (E : Entity_Id;
         S : Scope_Index) return Boolean;
      --  Check whether entity E is in SPARK_Scope_Table at index S or higher

      function Is_Global_Constant (E : Entity_Id) return Boolean;
      --  Return True if E is a global constant for which we should ignore
      --  reads in SPARK.

      function Lt (Op1 : Natural; Op2 : Natural) return Boolean;
      --  Comparison function for Sort call

      procedure Move (From : Natural; To : Natural);
      --  Move procedure for Sort call

      procedure Update_Scope_Range
        (S    : Scope_Index;
         From : Xref_Index;
         To   : Xref_Index);
      --  Update the scope which maps to S with the new range From .. To

      package Sorting is new GNAT.Heap_Sort_G (Move, Lt);

      function Get_Scope_Num (N : Entity_Id) return Nat;
      --  Return the scope number associated to entity N

      procedure Set_Scope_Num (N : Entity_Id; Num : Nat);
      --  Associate entity N to scope number Num

      No_Scope : constant Nat := 0;
      --  Initial scope counter

      type Scope_Rec is record
         Num    : Nat;
         Entity : Entity_Id;
      end record;
      --  Type used to relate an entity and a scope number

      package Scopes is new GNAT.HTable.Simple_HTable
        (Header_Num => Entity_Hashed_Range,
         Element    => Scope_Rec,
         No_Element => (Num => No_Scope, Entity => Empty),
         Key        => Entity_Id,
         Hash       => Entity_Hash,
         Equal      => "=");
      --  Package used to build a correspondance between entities and scope
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
      --  for the call to sort. When we sort the table, we move the entries in
      --  Rnums around, but we do not move the original table entries.

      ---------------------
      -- Entity_Of_Scope --
      ---------------------

      function Entity_Of_Scope (S : Scope_Index) return Entity_Id is
      begin
         return SPARK_Scope_Table.Table (S).Scope_Entity;
      end Entity_Of_Scope;

      ---------------------
      -- Get_Entity_Type --
      ---------------------

      function Get_Entity_Type (E : Entity_Id) return Character is
      begin
         case Ekind (E) is
            when E_Out_Parameter    => return '<';
            when E_In_Out_Parameter => return '=';
            when E_In_Parameter     => return '>';
            when others             => return '*';
         end case;
      end Get_Entity_Type;

      -------------------
      -- Get_Scope_Num --
      -------------------

      function Get_Scope_Num (N : Entity_Id) return Nat is
      begin
         return Scopes.Get (N).Num;
      end Get_Scope_Num;

      ------------------------
      -- Is_SPARK_Reference --
      ------------------------

      function Is_SPARK_Reference
        (E   : Entity_Id;
         Typ : Character) return Boolean
      is
      begin
         --  The only references of interest on callable entities are calls. On
         --  non-callable entities, the only references of interest are reads
         --  and writes.

         if Ekind (E) in Overloadable_Kind then
            return Typ = 's';

         --  References to constant objects are not considered in SPARK
         --  section, as these will be translated as constants in the
         --  intermediate language for formal verification, and should
         --  therefore never appear in frame conditions.

         elsif Is_Constant_Object (E) then
            return False;

         --  Objects of Task type or protected type are not SPARK references

         elsif Present (Etype (E))
           and then Ekind (Etype (E)) in Concurrent_Kind
         then
            return False;

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
      begin
         return Present (E)
           and then not Is_Generic_Unit (E)
           and then Renamed_Entity (E) = Empty
           and then Get_Scope_Num (E) /= No_Scope;
      end Is_SPARK_Scope;

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
               if SPARK_Scope_Table.Table (Index).Scope_Entity = E then
                  declare
                     Dummy : constant SPARK_Scope_Record :=
                               SPARK_Scope_Table.Table (Index);
                     pragma Unreferenced (Dummy);
                  begin
                     return True;
                  end;
               end if;
            end loop;

            return False;
         end Is_Past_Scope_Entity;

      --  Start of processing for Is_Future_Scope_Entity

      begin
         for Index in S .. SPARK_Scope_Table.Last loop
            if SPARK_Scope_Table.Table (Index).Scope_Entity = E then
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
      -- Is_Global_Constant --
      ------------------------

      function Is_Global_Constant (E : Entity_Id) return Boolean is
      begin
         return Ekind (E) = E_Constant
           and then Ekind_In (Scope (E), E_Package, E_Package_Body);
      end Is_Global_Constant;

      --------
      -- Lt --
      --------

      function Lt (Op1, Op2 : Natural) return Boolean is
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

      procedure Set_Scope_Num (N : Entity_Id; Num : Nat) is
      begin
         Scopes.Set (K => N, E => Scope_Rec'(Num => Num, Entity => N));
      end Set_Scope_Num;

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

      Col        : Nat;
      From_Index : Xref_Index;
      Line       : Nat;
      Loc        : Source_Ptr;
      Prev_Typ   : Character;
      Ref_Count  : Nat;
      Ref_Id     : Entity_Id;
      Ref_Name   : String_Ptr;
      Scope_Id   : Scope_Index;

   --  Start of processing for Add_SPARK_Xrefs

   begin
      for Index in SPARK_Scope_Table.First .. SPARK_Scope_Table.Last loop
         declare
            S : SPARK_Scope_Record renames SPARK_Scope_Table.Table (Index);
         begin
            Set_Scope_Num (S.Scope_Entity, S.Scope_Num);
         end;
      end loop;

      --  Set up the pointer vector for the sort

      for Index in 1 .. Nrefs loop
         Rnums (Index) := Index;
      end loop;

      for Index in Drefs.First .. Drefs.Last loop
         Xrefs.Append (Drefs.Table (Index));

         Nrefs         := Nrefs + 1;
         Rnums (Nrefs) := Xrefs.Last;
      end loop;

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
            Ref : Xref_Key renames Xrefs.Table (Rnums (Index)).Key;

         begin
            if SPARK_Entities (Ekind (Ref.Ent))
              and then SPARK_References (Ref.Typ)
              and then Is_SPARK_Scope (Ref.Ent_Scope)
              and then Is_SPARK_Scope (Ref.Ref_Scope)
              and then not Is_Global_Constant (Ref.Ent)
              and then Is_SPARK_Reference (Ref.Ent, Ref.Typ)

              --  Discard references from unknown scopes, e.g. generic scopes

              and then Get_Scope_Num (Ref.Ent_Scope) /= No_Scope
              and then Get_Scope_Num (Ref.Ref_Scope) /= No_Scope
            then
               Nrefs         := Nrefs + 1;
               Rnums (Nrefs) := Rnums (Index);
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
            if Xrefs.Table (Rnums (Index)) /=
               Xrefs.Table (Rnums (Nrefs))
            then
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
      Loc       := No_Location;
      Prev_Typ  := 'm';

      for Index in 1 .. Ref_Count loop
         declare
            Ref : Xref_Key renames Xrefs.Table (Rnums (Index)).Key;

         begin
            if Ref.Loc /= Loc
              or else (Prev_Typ = 'm' and then Ref.Typ = 'r')
            then
               Loc           := Ref.Loc;
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

      Ref_Id     := Empty;
      Scope_Id   := 1;
      From_Index := 1;

      --  Loop to output references

      for Refno in 1 .. Nrefs loop
         declare
            Ref_Entry : Xref_Entry renames Xrefs.Table (Rnums (Refno));
            Ref       : Xref_Key   renames Ref_Entry.Key;

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

            if Ref.Ent /= Ref_Id then
               Ref_Name := new String'(Unique_Name (Ref.Ent));
            end if;

            if Ref.Ent = Heap then
               Line := 0;
               Col  := 0;
            else
               Line := Int (Get_Logical_Line_Number (Ref_Entry.Def));
               Col  := Int (Get_Column_Number (Ref_Entry.Def));
            end if;

            SPARK_Xref_Table.Append (
              (Entity_Name => Ref_Name,
               Entity_Line => Line,
               Etype       => Get_Entity_Type (Ref.Ent),
               Entity_Col  => Col,
               File_Num    => Dependency_Num (Ref.Lun),
               Scope_Num   => Get_Scope_Num (Ref.Ref_Scope),
               Line        => Int (Get_Logical_Line_Number (Ref.Loc)),
               Rtype       => Ref.Typ,
               Col         => Int (Get_Column_Number (Ref.Loc))));
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
      D1 : Nat;
      D2 : Nat;

   begin
      --  Cross-references should have been computed first

      pragma Assert (Xrefs.Last /= 0);

      Initialize_SPARK_Tables;

      --  Generate file and scope SPARK cross-reference information

      D1 := 1;
      while D1 <= Num_Sdep loop

         --  In rare cases, when treating the library-level instantiation of a
         --  generic, two consecutive units refer to the same compilation unit
         --  node and entity. In that case, treat them as a single unit for the
         --  sake of SPARK cross references by passing to Add_SPARK_File.

         if D1 < Num_Sdep
           and then Cunit_Entity (Sdep_Table (D1)) =
                    Cunit_Entity (Sdep_Table (D1 + 1))
         then
            D2 := D1 + 1;
         else
            D2 := D1;
         end if;

         Add_SPARK_File
           (Ubody => Sdep_Table (D1),
            Uspec => Sdep_Table (D2),
            Dspec => D2);
         D1 := D2 + 1;
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
               Entity_Hash_Table.Set (Srec.Scope_Entity, S);
            end;
         end loop;

         --  Use the hash-table to locate spec entities

         for S in SPARK_Scope_Table.First .. SPARK_Scope_Table.Last loop
            declare
               Srec : SPARK_Scope_Record renames SPARK_Scope_Table.Table (S);

               Spec_Entity : constant Entity_Id :=
                               Unique_Entity (Srec.Scope_Entity);
               Spec_Scope  : constant Scope_Index :=
                               Entity_Hash_Table.Get (Spec_Entity);

            begin
               --  Generic spec may be missing in which case Spec_Scope is zero

               if Spec_Entity /= Srec.Scope_Entity
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

   --------------------------------
   -- Detect_And_Add_SPARK_Scope --
   --------------------------------

   procedure Detect_And_Add_SPARK_Scope (N : Node_Id) is
   begin
      if Nkind_In (N, N_Subprogram_Declaration,
                      N_Subprogram_Body,
                      N_Subprogram_Body_Stub,
                      N_Package_Declaration,
                      N_Package_Body)
      then
         Add_SPARK_Scope (N);
      end if;
   end Detect_And_Add_SPARK_Scope;

   -------------------------------------
   -- Enclosing_Subprogram_Or_Package --
   -------------------------------------

   function Enclosing_Subprogram_Or_Package (N : Node_Id) return Entity_Id is
      Result : Entity_Id;

   begin
      --  If N is the defining identifier for a subprogram, then return the
      --  enclosing subprogram or package, not this subprogram.

      if Nkind_In (N, N_Defining_Identifier, N_Defining_Operator_Symbol)
        and then Nkind (Parent (N)) in N_Subprogram_Specification
      then
         Result := Parent (Parent (Parent (N)));
      else
         Result := N;
      end if;

      while Present (Result) loop
         case Nkind (Result) is
            when N_Package_Specification =>
               Result := Defining_Unit_Name (Result);
               exit;

            when N_Package_Body =>
               Result := Defining_Unit_Name (Result);
               exit;

            when N_Subprogram_Specification =>
               Result := Defining_Unit_Name (Result);
               exit;

            when N_Subprogram_Declaration =>
               Result := Defining_Unit_Name (Specification (Result));
               exit;

            when N_Subprogram_Body =>
               Result := Defining_Unit_Name (Specification (Result));
               exit;

            --  The enclosing subprogram for a pre- or postconditions should be
            --  the subprogram to which the pragma is attached. This is not
            --  always the case in the AST, as the pragma may be declared after
            --  the declaration of the subprogram. Return Empty in this case.

            when N_Pragma =>
               if Get_Pragma_Id (Result) = Pragma_Precondition
                    or else
                  Get_Pragma_Id (Result) = Pragma_Postcondition
               then
                  return Empty;
               else
                  Result := Parent (Result);
               end if;

            when others =>
               Result := Parent (Result);
         end case;
      end loop;

      if Nkind (Result) = N_Defining_Program_Unit_Name then
         Result := Defining_Identifier (Result);
      end if;

      --  Do not return a scope without a proper location

      if Present (Result)
        and then Sloc (Result) = No_Location
      then
         return Empty;
      end if;

      return Result;
   end Enclosing_Subprogram_Or_Package;

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

      Loc       : constant Source_Ptr := Sloc (N);
      Index     : Nat;
      Ref_Scope : Entity_Id;

   --  Start of processing for Generate_Dereference

   begin

      if Loc > No_Location then
         Drefs.Increment_Last;
         Index := Drefs.Last;

         declare
            Deref_Entry : Xref_Entry renames Drefs.Table (Index);
            Deref       : Xref_Key   renames Deref_Entry.Key;

         begin
            if No (Heap) then
               Create_Heap;
            end if;

            Ref_Scope := Enclosing_Subprogram_Or_Package (N);

            Deref.Ent := Heap;
            Deref.Loc := Loc;
            Deref.Typ := Typ;

            --  It is as if the special "Heap" was defined in every scope where
            --  it is referenced.

            Deref.Eun := Get_Code_Unit (Loc);
            Deref.Lun := Get_Code_Unit (Loc);

            Deref.Ref_Scope := Ref_Scope;
            Deref.Ent_Scope := Ref_Scope;

            Deref_Entry.Def := No_Location;

            Deref_Entry.Ent_Scope_File := Get_Code_Unit (N);
         end;
      end if;
   end Generate_Dereference;

   ------------------------------------
   -- Traverse_All_Compilation_Units --
   ------------------------------------

   procedure Traverse_All_Compilation_Units (Process : Node_Processing) is
   begin
      for U in Units.First .. Last_Unit loop
         Traverse_Compilation_Unit (Cunit (U), Process, Inside_Stubs => False);
      end loop;
   end Traverse_All_Compilation_Units;

   -------------------------------
   -- Traverse_Compilation_Unit --
   -------------------------------

   procedure Traverse_Compilation_Unit
     (CU           : Node_Id;
      Process      : Node_Processing;
      Inside_Stubs : Boolean)
   is
      Lu : Node_Id;

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

      if Nkind (Lu) = N_Subprogram_Body then
         Traverse_Subprogram_Body (Lu, Process, Inside_Stubs);

      elsif Nkind (Lu) = N_Subprogram_Declaration then
         null;

      elsif Nkind (Lu) = N_Package_Declaration then
         Traverse_Package_Declaration (Lu, Process, Inside_Stubs);

      elsif Nkind (Lu) = N_Package_Body then
         Traverse_Package_Body (Lu, Process, Inside_Stubs);

      --  All other cases of compilation units (e.g. renamings), are not
      --  declarations, or else generic declarations which are ignored.

      else
         null;
      end if;
   end Traverse_Compilation_Unit;

   -----------------------------------------
   -- Traverse_Declarations_Or_Statements --
   -----------------------------------------

   procedure Traverse_Declarations_Or_Statements
     (L            : List_Id;
      Process      : Node_Processing;
      Inside_Stubs : Boolean)
   is
      N : Node_Id;

   begin
      --  Loop through statements or declarations

      N := First (L);
      while Present (N) loop
         --  Call Process on all declarations

         if Nkind (N) in N_Declaration
              or else
            Nkind (N) in N_Later_Decl_Item
         then
            Process (N);
         end if;

         case Nkind (N) is

            --  Package declaration

            when N_Package_Declaration =>
               Traverse_Package_Declaration (N, Process, Inside_Stubs);

            --  Package body

            when N_Package_Body =>
               if Ekind (Defining_Entity (N)) /= E_Generic_Package then
                  Traverse_Package_Body (N, Process, Inside_Stubs);
               end if;

            when N_Package_Body_Stub =>
               if Present (Library_Unit (N)) then
                  declare
                     Body_N : constant Node_Id := Get_Body_From_Stub (N);
                  begin
                     if Inside_Stubs
                       and then
                         Ekind (Defining_Entity (Body_N)) /= E_Generic_Package
                     then
                        Traverse_Package_Body (Body_N, Process, Inside_Stubs);
                     end if;
                  end;
               end if;

            --  Subprogram declaration

            when N_Subprogram_Declaration =>
               null;

            --  Subprogram body

            when N_Subprogram_Body =>
               if not Is_Generic_Subprogram (Defining_Entity (N)) then
                  Traverse_Subprogram_Body (N, Process, Inside_Stubs);
               end if;

            when N_Subprogram_Body_Stub =>
               if Present (Library_Unit (N)) then
                  declare
                     Body_N : constant Node_Id := Get_Body_From_Stub (N);
                  begin
                     if Inside_Stubs
                       and then
                         not Is_Generic_Subprogram (Defining_Entity (Body_N))
                     then
                        Traverse_Subprogram_Body
                          (Body_N, Process, Inside_Stubs);
                     end if;
                  end;
               end if;

            --  Block statement

            when N_Block_Statement =>
               Traverse_Declarations_Or_Statements
                 (Declarations (N), Process, Inside_Stubs);
               Traverse_Handled_Statement_Sequence
                 (Handled_Statement_Sequence (N), Process, Inside_Stubs);

            when N_If_Statement =>

               --  Traverse the statements in the THEN part

               Traverse_Declarations_Or_Statements
                 (Then_Statements (N), Process, Inside_Stubs);

               --  Loop through ELSIF parts if present

               if Present (Elsif_Parts (N)) then
                  declare
                     Elif : Node_Id := First (Elsif_Parts (N));

                  begin
                     while Present (Elif) loop
                        Traverse_Declarations_Or_Statements
                          (Then_Statements (Elif), Process, Inside_Stubs);
                        Next (Elif);
                     end loop;
                  end;
               end if;

               --  Finally traverse the ELSE statements if present

               Traverse_Declarations_Or_Statements
                 (Else_Statements (N), Process, Inside_Stubs);

            --  Case statement

            when N_Case_Statement =>

               --  Process case branches

               declare
                  Alt : Node_Id;
               begin
                  Alt := First (Alternatives (N));
                  while Present (Alt) loop
                     Traverse_Declarations_Or_Statements
                       (Statements (Alt), Process, Inside_Stubs);
                     Next (Alt);
                  end loop;
               end;

            --  Extended return statement

            when N_Extended_Return_Statement =>
               Traverse_Handled_Statement_Sequence
                 (Handled_Statement_Sequence (N), Process, Inside_Stubs);

            --  Loop

            when N_Loop_Statement =>
               Traverse_Declarations_Or_Statements
                 (Statements (N), Process, Inside_Stubs);

            --  Generic declarations are ignored

            when others =>
               null;
         end case;

         Next (N);
      end loop;
   end Traverse_Declarations_Or_Statements;

   -----------------------------------------
   -- Traverse_Handled_Statement_Sequence --
   -----------------------------------------

   procedure Traverse_Handled_Statement_Sequence
     (N            : Node_Id;
      Process      : Node_Processing;
      Inside_Stubs : Boolean)
   is
      Handler : Node_Id;

   begin
      if Present (N) then
         Traverse_Declarations_Or_Statements
           (Statements (N), Process, Inside_Stubs);

         if Present (Exception_Handlers (N)) then
            Handler := First (Exception_Handlers (N));
            while Present (Handler) loop
               Traverse_Declarations_Or_Statements
                 (Statements (Handler), Process, Inside_Stubs);
               Next (Handler);
            end loop;
         end if;
      end if;
   end Traverse_Handled_Statement_Sequence;

   ---------------------------
   -- Traverse_Package_Body --
   ---------------------------

   procedure Traverse_Package_Body
     (N            : Node_Id;
      Process      : Node_Processing;
      Inside_Stubs : Boolean) is
   begin
      Traverse_Declarations_Or_Statements
        (Declarations (N), Process, Inside_Stubs);
      Traverse_Handled_Statement_Sequence
        (Handled_Statement_Sequence (N), Process, Inside_Stubs);
   end Traverse_Package_Body;

   ----------------------------------
   -- Traverse_Package_Declaration --
   ----------------------------------

   procedure Traverse_Package_Declaration
     (N            : Node_Id;
      Process      : Node_Processing;
      Inside_Stubs : Boolean)
   is
      Spec : constant Node_Id := Specification (N);
   begin
      Traverse_Declarations_Or_Statements
        (Visible_Declarations (Spec), Process, Inside_Stubs);
      Traverse_Declarations_Or_Statements
        (Private_Declarations (Spec), Process, Inside_Stubs);
   end Traverse_Package_Declaration;

   ------------------------------
   -- Traverse_Subprogram_Body --
   ------------------------------

   procedure Traverse_Subprogram_Body
     (N            : Node_Id;
      Process      : Node_Processing;
      Inside_Stubs : Boolean)
   is
   begin
      Traverse_Declarations_Or_Statements
        (Declarations (N), Process, Inside_Stubs);
      Traverse_Handled_Statement_Sequence
        (Handled_Statement_Sequence (N), Process, Inside_Stubs);
   end Traverse_Subprogram_Body;

end SPARK_Specific;
