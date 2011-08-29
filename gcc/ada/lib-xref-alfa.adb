------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        L I B . X R E F . A L F A                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2011, Free Software Foundation, Inc.           --
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

with ALFA;        use ALFA;
with Einfo;       use Einfo;
with Put_ALFA;
with GNAT.HTable;

separate (Lib.Xref)
package body ALFA is

   ---------------------
   -- Local Constants --
   ---------------------

   --  Table of ALFA_Entities, True for each entity kind used in ALFA

   ALFA_Entities : constant array (Entity_Kind) of Boolean :=
     (E_Void                                       => False,
      E_Variable                                   => True,
      E_Component                                  => False,
      E_Constant                                   => True,
      E_Discriminant                               => False,

      E_Loop_Parameter                             => True,
      E_In_Parameter                               => True,
      E_Out_Parameter                              => True,
      E_In_Out_Parameter                           => True,
      E_Generic_In_Out_Parameter                   => False,

      E_Generic_In_Parameter                       => False,
      E_Named_Integer                              => False,
      E_Named_Real                                 => False,
      E_Enumeration_Type                           => False,
      E_Enumeration_Subtype                        => False,

      E_Signed_Integer_Type                        => False,
      E_Signed_Integer_Subtype                     => False,
      E_Modular_Integer_Type                       => False,
      E_Modular_Integer_Subtype                    => False,
      E_Ordinary_Fixed_Point_Type                  => False,

      E_Ordinary_Fixed_Point_Subtype               => False,
      E_Decimal_Fixed_Point_Type                   => False,
      E_Decimal_Fixed_Point_Subtype                => False,
      E_Floating_Point_Type                        => False,
      E_Floating_Point_Subtype                     => False,

      E_Access_Type                                => False,
      E_Access_Subtype                             => False,
      E_Access_Attribute_Type                      => False,
      E_Allocator_Type                             => False,
      E_General_Access_Type                        => False,

      E_Access_Subprogram_Type                     => False,
      E_Access_Protected_Subprogram_Type           => False,
      E_Anonymous_Access_Subprogram_Type           => False,
      E_Anonymous_Access_Protected_Subprogram_Type => False,
      E_Anonymous_Access_Type                      => False,

      E_Array_Type                                 => False,
      E_Array_Subtype                              => False,
      E_String_Type                                => False,
      E_String_Subtype                             => False,
      E_String_Literal_Subtype                     => False,

      E_Class_Wide_Type                            => False,
      E_Class_Wide_Subtype                         => False,
      E_Record_Type                                => False,
      E_Record_Subtype                             => False,
      E_Record_Type_With_Private                   => False,

      E_Record_Subtype_With_Private                => False,
      E_Private_Type                               => False,
      E_Private_Subtype                            => False,
      E_Limited_Private_Type                       => False,
      E_Limited_Private_Subtype                    => False,

      E_Incomplete_Type                            => False,
      E_Incomplete_Subtype                         => False,
      E_Task_Type                                  => False,
      E_Task_Subtype                               => False,
      E_Protected_Type                             => False,

      E_Protected_Subtype                          => False,
      E_Exception_Type                             => False,
      E_Subprogram_Type                            => False,
      E_Enumeration_Literal                        => False,
      E_Function                                   => True,

      E_Operator                                   => True,
      E_Procedure                                  => True,
      E_Entry                                      => False,
      E_Entry_Family                               => False,
      E_Block                                      => False,

      E_Entry_Index_Parameter                      => False,
      E_Exception                                  => False,
      E_Generic_Function                           => False,
      E_Generic_Package                            => False,
      E_Generic_Procedure                          => False,

      E_Label                                      => False,
      E_Loop                                       => False,
      E_Return_Statement                           => False,
      E_Package                                    => False,

      E_Package_Body                               => False,
      E_Protected_Object                           => False,
      E_Protected_Body                             => False,
      E_Task_Body                                  => False,
      E_Subprogram_Body                            => False);

   --  True for each reference type used in ALFA
   ALFA_References : constant array (Character) of Boolean :=
     ('m' => True,
      'r' => True,
      's' => True,
      others => False);

   type Entity_Hashed_Range is range 0 .. 255;
   --  Size of hash table headers

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Add_ALFA_File (U : Unit_Number_Type; D : Nat);
   --  Add file U and all scopes in U to the tables ALFA_File_Table and
   --  ALFA_Scope_Table.

   procedure Add_ALFA_Scope (N : Node_Id);
   --  Add scope N to the table ALFA_Scope_Table

   procedure Add_ALFA_Xrefs;
   --  Filter table Xrefs to add all references used in ALFA to the table
   --  ALFA_Xref_Table.

   procedure Detect_And_Add_ALFA_Scope (N : Node_Id);
   --  Call Add_ALFA_Scope on scopes

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
   --  Traverse the corresponding constructs, calling Process on all
   --  declarations.

   -------------------
   -- Add_ALFA_File --
   -------------------

   procedure Add_ALFA_File (U : Unit_Number_Type; D : Nat) is
      From : Scope_Index;

      S : constant Source_File_Index := Source_Index (U);

   begin
      --  Source file could be inexistant as a result of an error, if option
      --  gnatQ is used.

      if S = No_Source_File then
         return;
      end if;

      From := ALFA_Scope_Table.Last + 1;

      Traverse_Compilation_Unit (Cunit (U), Detect_And_Add_ALFA_Scope'Access,
                                 Inside_Stubs => False);

      --  Update scope numbers

      declare
         Count : Nat;

      begin
         Count := 1;
         for S in From .. ALFA_Scope_Table.Last loop
            declare
               E : Entity_Id renames ALFA_Scope_Table.Table (S).Scope_Entity;

            begin
               if Lib.Get_Source_Unit (E) = U then
                  ALFA_Scope_Table.Table (S).Scope_Num := Count;
                  ALFA_Scope_Table.Table (S).File_Num  := D;
                  Count                                := Count + 1;

               else
                  --  Mark for removal a scope S which is not located in unit
                  --  U, for example for scope inside generics that get
                  --  instantiated.

                  ALFA_Scope_Table.Table (S).Scope_Num := 0;
               end if;
            end;
         end loop;
      end;

      declare
         Snew : Scope_Index;

      begin
         Snew := From;
         for S in From .. ALFA_Scope_Table.Last loop
            --  Remove those scopes previously marked for removal

            if ALFA_Scope_Table.Table (S).Scope_Num /= 0 then
               ALFA_Scope_Table.Table (Snew) := ALFA_Scope_Table.Table (S);
               Snew := Snew + 1;
            end if;
         end loop;

         ALFA_Scope_Table.Set_Last (Snew - 1);
      end;

      --  Make entry for new file in file table

      Get_Name_String (Reference_Name (S));

      ALFA_File_Table.Append (
        (File_Name  => new String'(Name_Buffer (1 .. Name_Len)),
         File_Num   => D,
         From_Scope => From,
         To_Scope   => ALFA_Scope_Table.Last));
   end Add_ALFA_File;

   --------------------
   -- Add_ALFA_Scope --
   --------------------

   procedure Add_ALFA_Scope (N : Node_Id) is
      E   : constant Entity_Id  := Defining_Entity (N);
      Loc : constant Source_Ptr := Sloc (E);
      Typ : Character;

   begin
      --  Ignore scopes without a proper location

      if Sloc (N) = No_Location then
         return;
      end if;

      case Ekind (E) is
         when E_Function =>
            Typ := 'V';

         when E_Procedure =>
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

         when E_Package | E_Package_Body =>
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

      ALFA_Scope_Table.Append (
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
   end Add_ALFA_Scope;

   --------------------
   -- Add_ALFA_Xrefs --
   --------------------

   procedure Add_ALFA_Xrefs is
      Cur_Scope_Idx   : Scope_Index;
      From_Xref_Idx   : Xref_Index;
      Cur_Entity      : Entity_Id;
      Cur_Entity_Name : String_Ptr;

      package Scopes is
         No_Scope : constant Nat := 0;
         function Get_Scope_Num (N : Entity_Id) return Nat;
         procedure Set_Scope_Num (N : Entity_Id; Num : Nat);
      end Scopes;

      ------------
      -- Scopes --
      ------------

      package body Scopes is
         type Scope is record
            Num    : Nat;
            Entity : Entity_Id;
         end record;

         package Scopes is new GNAT.HTable.Simple_HTable
           (Header_Num => Entity_Hashed_Range,
            Element    => Scope,
            No_Element => (Num => No_Scope, Entity => Empty),
            Key        => Entity_Id,
            Hash       => Entity_Hash,
            Equal      => "=");

         -------------------
         -- Get_Scope_Num --
         -------------------

         function Get_Scope_Num (N : Entity_Id) return Nat is
         begin
            return Scopes.Get (N).Num;
         end Get_Scope_Num;

         -------------------
         -- Set_Scope_Num --
         -------------------

         procedure Set_Scope_Num (N : Entity_Id; Num : Nat) is
         begin
            Scopes.Set (K => N, E => Scope'(Num => Num, Entity => N));
         end Set_Scope_Num;
      end Scopes;

      use Scopes;

      Nrefs : Nat := Xrefs.Last;
      --  Number of references in table. This value may get reset (reduced)
      --  when we eliminate duplicate reference entries as well as references
      --  not suitable for local cross-references.

      Rnums : array (0 .. Nrefs) of Nat;
      --  This array contains numbers of references in the Xrefs table. This
      --  list is sorted in output order. The extra 0'th entry is convenient
      --  for the call to sort. When we sort the table, we move the entries in
      --  Rnums around, but we do not move the original table entries.

      function Lt (Op1, Op2 : Natural) return Boolean;
      --  Comparison function for Sort call

      procedure Move (From : Natural; To : Natural);
      --  Move procedure for Sort call

      package Sorting is new GNAT.Heap_Sort_G (Move, Lt);

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

         elsif Get_Scope_Num (T1.Ent_Scope) /=
           Get_Scope_Num (T2.Ent_Scope)
         then
            return Get_Scope_Num (T1.Ent_Scope) < Get_Scope_Num (T2.Ent_Scope);

         --  Third test: within same unit and scope, sort by location of
         --  entity definition.

         elsif T1.Def /= T2.Def then
            return T1.Def < T2.Def;

         --  Fourth test: if reference is in same unit as entity definition,
         --  sort first.

         elsif T1.Lun /= T2.Lun and then T1.Ent_Scope_File = T1.Lun then
            return True;
         elsif T1.Lun /= T2.Lun and then T2.Ent_Scope_File = T2.Lun then
            return False;

         --  Fifth test: if reference is in same unit and same scope as entity
         --  definition, sort first.

         elsif T1.Ent_Scope_File = T1.Lun
           and then T1.Ref_Scope /= T2.Ref_Scope
           and then T1.Ent_Scope = T1.Ref_Scope
         then
            return True;
         elsif T1.Ent_Scope_File = T1.Lun
           and then T1.Ref_Scope /= T2.Ref_Scope
           and then T2.Ent_Scope = T2.Ref_Scope
         then
            return False;

         --  Sixth test: for same entity, sort by reference location unit

         elsif T1.Lun /= T2.Lun then
            return Dependency_Num (T1.Lun) < Dependency_Num (T2.Lun);

         --  Seventh test: for same entity, sort by reference location scope

         elsif Get_Scope_Num (T1.Ref_Scope) /=
           Get_Scope_Num (T2.Ref_Scope)
         then
            return Get_Scope_Num (T1.Ref_Scope) < Get_Scope_Num (T2.Ref_Scope);

         --  Eighth test: order of location within referencing unit

         elsif T1.Loc /= T2.Loc then
            return T1.Loc < T2.Loc;

         --  Finally, for two locations at the same address prefer the one that
         --  does NOT have the type 'r', so that a modification or extension
         --  takes preference, when there are more than one reference at the
         --  same location. As a result, in the case of entities that are
         --  in-out actuals, the read reference follows the modify reference.

         else
            return T2.Typ = 'r';
         end if;
      end Lt;

      ----------
      -- Move --
      ----------

      procedure Move (From : Natural; To : Natural) is
      begin
         Rnums (Nat (To)) := Rnums (Nat (From));
      end Move;

      --  Start of processing for Add_ALFA_Xrefs
   begin

      for J in ALFA_Scope_Table.First .. ALFA_Scope_Table.Last loop
         Set_Scope_Num (N   => ALFA_Scope_Table.Table (J).Scope_Entity,
                        Num => ALFA_Scope_Table.Table (J).Scope_Num);
      end loop;

      --  Set up the pointer vector for the sort

      for J in 1 .. Nrefs loop
         Rnums (J) := J;
      end loop;

      --  Eliminate entries not appropriate for ALFA. Done prior to sorting
      --  cross-references, as it discards useless references which do not have
      --  a proper format for the comparison function (like no location).

      Eliminate_Before_Sort : declare
         NR : Nat;

         function Is_ALFA_Scope (E : Entity_Id) return Boolean;
         --  Return whether the entity or reference scope is adequate

         function Is_Global_Constant (E : Entity_Id) return Boolean;
         --  Return True if E is a global constant for which we should ignore
         --  reads in ALFA.

         -------------------
         -- Is_ALFA_Scope --
         -------------------

         function Is_ALFA_Scope (E : Entity_Id) return Boolean is
         begin
            return Present (E)
              and then not Is_Generic_Unit (E)
              and then Renamed_Entity (E) = Empty
              and then Get_Scope_Num (E) /= No_Scope;
         end Is_ALFA_Scope;

         ------------------------
         -- Is_Global_Constant --
         ------------------------

         function Is_Global_Constant (E : Entity_Id) return Boolean is
         begin
            return Ekind (E) = E_Constant
              and then Ekind_In (Scope (E), E_Package, E_Package_Body);
         end Is_Global_Constant;

      --  Start of processing for Eliminate_Before_Sort

      begin
         NR    := Nrefs;
         Nrefs := 0;

         for J in 1 .. NR loop
            if ALFA_Entities (Ekind (Xrefs.Table (Rnums (J)).Ent))
              and then ALFA_References (Xrefs.Table (Rnums (J)).Typ)
              and then Is_ALFA_Scope (Xrefs.Table (Rnums (J)).Ent_Scope)
              and then Is_ALFA_Scope (Xrefs.Table (Rnums (J)).Ref_Scope)
              and then not Is_Global_Constant (Xrefs.Table (Rnums (J)).Ent)
            then
               Nrefs         := Nrefs + 1;
               Rnums (Nrefs) := Rnums (J);
            end if;
         end loop;
      end Eliminate_Before_Sort;

      --  Sort the references

      Sorting.Sort (Integer (Nrefs));

      Eliminate_After_Sort : declare
         NR : Nat;

         Crloc : Source_Ptr;
         --  Current reference location

         Prevt : Character;
         --  reference kind of previous reference

      begin
         --  Eliminate duplicate entries

         --  We need this test for NR because if we force ALI file generation
         --  in case of errors detected, it may be the case that Nrefs is 0, so
         --  we should not reset it here

         if Nrefs >= 2 then
            NR    := Nrefs;
            Nrefs := 1;

            for J in 2 .. NR loop
               if Xrefs.Table (Rnums (J)) /=
                 Xrefs.Table (Rnums (Nrefs))
               then
                  Nrefs := Nrefs + 1;
                  Rnums (Nrefs) := Rnums (J);
               end if;
            end loop;
         end if;

         --  Eliminate the reference if it is at the same location as the
         --  previous one, unless it is a read-reference indicating that the
         --  entity is an in-out actual in a call.

         NR    := Nrefs;
         Nrefs := 0;
         Crloc := No_Location;
         Prevt := 'm';

         for J in 1 .. NR loop
            if Xrefs.Table (Rnums (J)).Loc /= Crloc
              or else (Prevt = 'm'
                        and then Xrefs.Table (Rnums (J)).Typ = 'r')
            then
               Crloc         := Xrefs.Table (Rnums (J)).Loc;
               Prevt         := Xrefs.Table (Rnums (J)).Typ;
               Nrefs         := Nrefs + 1;
               Rnums (Nrefs) := Rnums (J);
            end if;
         end loop;
      end Eliminate_After_Sort;

      --  Initialize loop

      Cur_Scope_Idx  := 1;
      From_Xref_Idx  := 1;
      Cur_Entity     := Empty;

      if ALFA_Scope_Table.Last = 0 then
         return;
      end if;

      --  Loop to output references

      for Refno in 1 .. Nrefs loop
         Add_One_Xref : declare

            -----------------------
            -- Local Subprograms --
            -----------------------

            function Cur_Scope return Node_Id;
            --  Return scope entity which corresponds to index Cur_Scope_Idx in
            --  table ALFA_Scope_Table.

            function Get_Entity_Type (E : Entity_Id) return Character;
            --  Return a character representing the type of entity

            function Is_Future_Scope_Entity (E : Entity_Id) return Boolean;
            --  Check whether entity E is in ALFA_Scope_Table at index
            --  Cur_Scope_Idx or higher.

            function Is_Past_Scope_Entity (E : Entity_Id) return Boolean;
            --  Check whether entity E is in ALFA_Scope_Table at index strictly
            --  lower than Cur_Scope_Idx.

            ---------------
            -- Cur_Scope --
            ---------------

            function Cur_Scope return Node_Id is
            begin
               return ALFA_Scope_Table.Table (Cur_Scope_Idx).Scope_Entity;
            end Cur_Scope;

            ---------------------
            -- Get_Entity_Type --
            ---------------------

            function Get_Entity_Type (E : Entity_Id) return Character is
               C : Character;
            begin
               case Ekind (E) is
                  when E_Out_Parameter    => C := '<';
                  when E_In_Out_Parameter => C := '=';
                  when E_In_Parameter     => C := '>';
                  when others             => C := '*';
               end case;
               return C;
            end Get_Entity_Type;

            ----------------------------
            -- Is_Future_Scope_Entity --
            ----------------------------

            function Is_Future_Scope_Entity (E : Entity_Id) return Boolean is
            begin
               for J in Cur_Scope_Idx .. ALFA_Scope_Table.Last loop
                  if E = ALFA_Scope_Table.Table (J).Scope_Entity then
                     return True;
                  end if;
               end loop;

               --  If this assertion fails, this means that the scope which we
               --  are looking for has been treated already, which reveals a
               --  problem in the order of cross-references.

               pragma Assert (not Is_Past_Scope_Entity (E));

               return False;
            end Is_Future_Scope_Entity;

            --------------------------
            -- Is_Past_Scope_Entity --
            --------------------------

            function Is_Past_Scope_Entity (E : Entity_Id) return Boolean is
            begin
               for J in ALFA_Scope_Table.First .. Cur_Scope_Idx - 1 loop
                  if E = ALFA_Scope_Table.Table (J).Scope_Entity then
                     return True;
                  end if;
               end loop;

               return False;
            end Is_Past_Scope_Entity;

            ---------------------
            -- Local Variables --
            ---------------------

            XE  : Xref_Entry renames Xrefs.Table (Rnums (Refno));

         begin
            --  If this assertion fails, the scope which we are looking for is
            --  not in ALFA scope table, which reveals either a problem in the
            --  construction of the scope table, or an erroneous scope for the
            --  current cross-reference.

            pragma Assert (Is_Future_Scope_Entity (XE.Ent_Scope));

            --  Update the range of cross references to which the current scope
            --  refers to. This may be the empty range only for the first scope
            --  considered.

            if XE.Ent_Scope /= Cur_Scope then
               ALFA_Scope_Table.Table (Cur_Scope_Idx).From_Xref :=
                 From_Xref_Idx;
               ALFA_Scope_Table.Table (Cur_Scope_Idx).To_Xref :=
                 ALFA_Xref_Table.Last;
               From_Xref_Idx := ALFA_Xref_Table.Last + 1;
            end if;

            while XE.Ent_Scope /= Cur_Scope loop
               Cur_Scope_Idx := Cur_Scope_Idx + 1;
               pragma Assert (Cur_Scope_Idx <= ALFA_Scope_Table.Last);
            end loop;

            if XE.Ent /= Cur_Entity then
               Cur_Entity_Name :=
                 new String'(Unique_Name (XE.Ent));
            end if;

            ALFA_Xref_Table.Append (
              (Entity_Name => Cur_Entity_Name,
               Entity_Line => Int (Get_Logical_Line_Number (XE.Def)),
               Etype       => Get_Entity_Type (XE.Ent),
               Entity_Col  => Int (Get_Column_Number (XE.Def)),
               File_Num    => Dependency_Num (XE.Lun),
               Scope_Num   => Get_Scope_Num (XE.Ref_Scope),
               Line        => Int (Get_Logical_Line_Number (XE.Loc)),
               Rtype       => XE.Typ,
               Col         => Int (Get_Column_Number (XE.Loc))));
         end Add_One_Xref;
      end loop;

      --  Update the range of cross references to which the scope refers to

      ALFA_Scope_Table.Table (Cur_Scope_Idx).From_Xref := From_Xref_Idx;
      ALFA_Scope_Table.Table (Cur_Scope_Idx).To_Xref   := ALFA_Xref_Table.Last;
   end Add_ALFA_Xrefs;

   ------------------
   -- Collect_ALFA --
   ------------------

   procedure Collect_ALFA (Sdep_Table : Unit_Ref_Table; Num_Sdep : Nat) is
   begin
      --  Cross-references should have been computed first

      pragma Assert (Xrefs.Last /= 0);

      Initialize_ALFA_Tables;

      --  Generate file and scope ALFA information

      for D in 1 .. Num_Sdep loop

         --  Ignore file for System

         if Units.Table (Sdep_Table (D)).Source_Index /=
           System_Source_File_Index
         then
            Add_ALFA_File (U => Sdep_Table (D), D => D);
         end if;
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

         for S in ALFA_Scope_Table.First .. ALFA_Scope_Table.Last loop
            declare
               Srec : ALFA_Scope_Record renames ALFA_Scope_Table.Table (S);
            begin
               Entity_Hash_Table.Set (Srec.Scope_Entity, S);
            end;
         end loop;

         --  Use the hash-table to locate spec entities

         for S in ALFA_Scope_Table.First .. ALFA_Scope_Table.Last loop
            declare
               Srec : ALFA_Scope_Record renames ALFA_Scope_Table.Table (S);

               Body_Entity : Entity_Id;
               Spec_Entity : Entity_Id;
               Spec_Scope  : Scope_Index;

            begin
               if Ekind (Srec.Scope_Entity) = E_Subprogram_Body then
                  Body_Entity := Parent (Parent (Srec.Scope_Entity));
               elsif Ekind (Srec.Scope_Entity) = E_Package_Body then
                  Body_Entity := Parent (Srec.Scope_Entity);
               else
                  Body_Entity := Empty;
               end if;

               if Present (Body_Entity) then
                  if Nkind (Body_Entity) = N_Defining_Program_Unit_Name then
                     Body_Entity := Parent (Body_Entity);
                  elsif Nkind (Body_Entity) = N_Subprogram_Body_Stub then
                     Body_Entity :=
                       Proper_Body (Unit (Library_Unit (Body_Entity)));
                  end if;

                  Spec_Entity := Corresponding_Spec (Body_Entity);
                  Spec_Scope := Entity_Hash_Table.Get (Spec_Entity);

                  --  Spec of generic may be missing

                  if Spec_Scope /= 0 then
                     Srec.Spec_File_Num :=
                       ALFA_Scope_Table.Table (Spec_Scope).File_Num;
                     Srec.Spec_Scope_Num :=
                       ALFA_Scope_Table.Table (Spec_Scope).Scope_Num;
                  end if;
               end if;
            end;
         end loop;
      end;

      --  Generate cross reference ALFA information

      Add_ALFA_Xrefs;
   end Collect_ALFA;

   -------------------------------
   -- Detect_And_Add_ALFA_Scope --
   -------------------------------

   procedure Detect_And_Add_ALFA_Scope (N : Node_Id) is
   begin
      if Nkind_In (N, N_Subprogram_Declaration,
                      N_Subprogram_Body,
                      N_Subprogram_Body_Stub,
                      N_Package_Declaration,
                      N_Package_Body)
      then
         Add_ALFA_Scope (N);
      end if;
   end Detect_And_Add_ALFA_Scope;

   -----------------
   -- Entity_Hash --
   -----------------

   function Entity_Hash (E : Entity_Id) return Entity_Hashed_Range is
   begin
      return
        Entity_Hashed_Range (E mod (Entity_Id (Entity_Hashed_Range'Last) + 1));
   end Entity_Hash;

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

      --  ??? TBD

      elsif Nkind (Lu) = N_Generic_Package_Declaration then
         null;

      --  ??? TBD

      elsif Nkind (Lu) in N_Generic_Instantiation then
         null;

      --  All other cases of compilation units (e.g. renamings), are not
      --  declarations.

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

            --  Generic package declaration ??? TBD

            when N_Generic_Package_Declaration =>
               null;

            --  Package body

            when N_Package_Body =>
               if Ekind (Defining_Entity (N)) /= E_Generic_Package then
                  Traverse_Package_Body (N, Process, Inside_Stubs);
               end if;

            when N_Package_Body_Stub =>
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

            --  Subprogram declaration

            when N_Subprogram_Declaration =>
               null;

            --  Generic subprogram declaration ??? TBD

            when N_Generic_Subprogram_Declaration =>
               null;

            --  Subprogram body

            when N_Subprogram_Body =>
               if not Is_Generic_Subprogram (Defining_Entity (N)) then
                  Traverse_Subprogram_Body (N, Process, Inside_Stubs);
               end if;

            when N_Subprogram_Body_Stub =>
               declare
                  Body_N : constant Node_Id := Get_Body_From_Stub (N);
               begin
                  if Inside_Stubs
                    and then
                      not Is_Generic_Subprogram (Defining_Entity (Body_N))
                  then
                     Traverse_Subprogram_Body (Body_N, Process, Inside_Stubs);
                  end if;
               end;

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
      Inside_Stubs : Boolean) is
   begin
      Traverse_Declarations_Or_Statements
        (Declarations (N), Process, Inside_Stubs);
      Traverse_Handled_Statement_Sequence
        (Handled_Statement_Sequence (N), Process, Inside_Stubs);
   end Traverse_Subprogram_Body;

end ALFA;
