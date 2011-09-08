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

with Alfa;     use Alfa;
with Einfo;    use Einfo;
with Nmake;    use Nmake;
with Put_Alfa;

with GNAT.HTable;

separate (Lib.Xref)
package body Alfa is

   ---------------------
   -- Local Constants --
   ---------------------

   --  Table of Alfa_Entities, True for each entity kind used in Alfa

   Alfa_Entities : constant array (Entity_Kind) of Boolean :=
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

   --  True for each reference type used in Alfa
   Alfa_References : constant array (Character) of Boolean :=
     ('m' => True,
      'r' => True,
      's' => True,
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
     Table_Initial        => Alloc.Xrefs_Initial,
     Table_Increment      => Alloc.Xrefs_Increment,
     Table_Name           => "Drefs");
   --  Table of cross-references for reads and writes through explicit
   --  dereferences, that are output as reads/writes to the special variable
   --  "Heap". These references are added to the regular references when
   --  computing Alfa cross-references.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Add_Alfa_File (U : Unit_Number_Type; D : Nat);
   --  Add file U and all scopes in U to the tables Alfa_File_Table and
   --  Alfa_Scope_Table.

   procedure Add_Alfa_Scope (N : Node_Id);
   --  Add scope N to the table Alfa_Scope_Table

   procedure Add_Alfa_Xrefs;
   --  Filter table Xrefs to add all references used in Alfa to the table
   --  Alfa_Xref_Table.

   procedure Detect_And_Add_Alfa_Scope (N : Node_Id);
   --  Call Add_Alfa_Scope on scopes

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
   -- Add_Alfa_File --
   -------------------

   procedure Add_Alfa_File (U : Unit_Number_Type; D : Nat) is
      From : Scope_Index;

      S : constant Source_File_Index := Source_Index (U);

   begin
      --  Source file could be inexistant as a result of an error, if option
      --  gnatQ is used.

      if S = No_Source_File then
         return;
      end if;

      From := Alfa_Scope_Table.Last + 1;

      Traverse_Compilation_Unit (Cunit (U), Detect_And_Add_Alfa_Scope'Access,
                                 Inside_Stubs => False);

      --  Update scope numbers

      declare
         Count : Nat;

      begin
         Count := 1;
         for S in From .. Alfa_Scope_Table.Last loop
            declare
               E : Entity_Id renames Alfa_Scope_Table.Table (S).Scope_Entity;

            begin
               if Lib.Get_Source_Unit (E) = U then
                  Alfa_Scope_Table.Table (S).Scope_Num := Count;
                  Alfa_Scope_Table.Table (S).File_Num  := D;
                  Count                                := Count + 1;

               else
                  --  Mark for removal a scope S which is not located in unit
                  --  U, for example for scope inside generics that get
                  --  instantiated.

                  Alfa_Scope_Table.Table (S).Scope_Num := 0;
               end if;
            end;
         end loop;
      end;

      declare
         Snew : Scope_Index;

      begin
         Snew := From;
         for S in From .. Alfa_Scope_Table.Last loop
            --  Remove those scopes previously marked for removal

            if Alfa_Scope_Table.Table (S).Scope_Num /= 0 then
               Alfa_Scope_Table.Table (Snew) := Alfa_Scope_Table.Table (S);
               Snew := Snew + 1;
            end if;
         end loop;

         Alfa_Scope_Table.Set_Last (Snew - 1);
      end;

      --  Make entry for new file in file table

      Get_Name_String (Reference_Name (S));

      Alfa_File_Table.Append (
        (File_Name  => new String'(Name_Buffer (1 .. Name_Len)),
         File_Num   => D,
         From_Scope => From,
         To_Scope   => Alfa_Scope_Table.Last));
   end Add_Alfa_File;

   --------------------
   -- Add_Alfa_Scope --
   --------------------

   procedure Add_Alfa_Scope (N : Node_Id) is
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

      Alfa_Scope_Table.Append (
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
   end Add_Alfa_Scope;

   --------------------
   -- Add_Alfa_Xrefs --
   --------------------

   procedure Add_Alfa_Xrefs is
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

      Nrefs_Add : constant Nat := Drefs.Last;

      Rnums : array (0 .. Nrefs + Nrefs_Add) of Nat;
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

         elsif Get_Scope_Num (T1.Key.Ent_Scope) /=
               Get_Scope_Num (T2.Key.Ent_Scope)
         then
            return Get_Scope_Num (T1.Key.Ent_Scope) <
              Get_Scope_Num (T2.Key.Ent_Scope);

         --  Third test: within same unit and scope, sort by location of
         --  entity definition.

         elsif T1.Def /= T2.Def then
            return T1.Def < T2.Def;

         --  Fourth test: if reference is in same unit as entity definition,
         --  sort first.

         elsif
           T1.Key.Lun /= T2.Key.Lun and then T1.Ent_Scope_File = T1.Key.Lun
         then
            return True;

         elsif
           T1.Key.Lun /= T2.Key.Lun and then T2.Ent_Scope_File = T2.Key.Lun
         then
            return False;

         --  Fifth test: if reference is in same unit and same scope as entity
         --  definition, sort first.

         elsif T1.Ent_Scope_File = T1.Key.Lun
           and then T1.Key.Ref_Scope /= T2.Key.Ref_Scope
           and then T1.Key.Ent_Scope = T1.Key.Ref_Scope
         then
            return True;
         elsif T1.Ent_Scope_File = T1.Key.Lun
           and then T1.Key.Ref_Scope /= T2.Key.Ref_Scope
           and then T2.Key.Ent_Scope = T2.Key.Ref_Scope
         then
            return False;

         --  Sixth test: for same entity, sort by reference location unit

         elsif T1.Key.Lun /= T2.Key.Lun then
            return Dependency_Num (T1.Key.Lun) < Dependency_Num (T2.Key.Lun);

         --  Seventh test: for same entity, sort by reference location scope

         elsif Get_Scope_Num (T1.Key.Ref_Scope) /=
               Get_Scope_Num (T2.Key.Ref_Scope)
         then
            return Get_Scope_Num (T1.Key.Ref_Scope) <
              Get_Scope_Num (T2.Key.Ref_Scope);

         --  Eighth test: order of location within referencing unit

         elsif T1.Key.Loc /= T2.Key.Loc then
            return T1.Key.Loc < T2.Key.Loc;

         --  Finally, for two locations at the same address prefer the one that
         --  does NOT have the type 'r', so that a modification or extension
         --  takes preference, when there are more than one reference at the
         --  same location. As a result, in the case of entities that are
         --  in-out actuals, the read reference follows the modify reference.

         else
            return T2.Key.Typ = 'r';
         end if;
      end Lt;

      ----------
      -- Move --
      ----------

      procedure Move (From : Natural; To : Natural) is
      begin
         Rnums (Nat (To)) := Rnums (Nat (From));
      end Move;

      Heap : Entity_Id;

   --  Start of processing for Add_Alfa_Xrefs

   begin
      for J in Alfa_Scope_Table.First .. Alfa_Scope_Table.Last loop
         Set_Scope_Num (N   => Alfa_Scope_Table.Table (J).Scope_Entity,
                        Num => Alfa_Scope_Table.Table (J).Scope_Num);
      end loop;

      --  Set up the pointer vector for the sort

      for J in 1 .. Nrefs loop
         Rnums (J) := J;
      end loop;

      --  Add dereferences to the set of regular references, by creating a
      --  special "Heap" variable for these special references.

      Name_Len := Name_Of_Heap_Variable'Length;
      Name_Buffer (1 .. Name_Len) := Name_Of_Heap_Variable;

      Atree.Unlock;
      Nlists.Unlock;
      Heap := Make_Defining_Identifier (Standard_Location, Name_Enter);
      Atree.Lock;
      Nlists.Lock;

      Set_Ekind         (Heap, E_Variable);
      Set_Is_Internal   (Heap, True);
      Set_Has_Fully_Qualified_Name (Heap);

      for J in Drefs.First .. Drefs.Last loop
         Xrefs.Append (Drefs.Table (J));

         --  Set entity at this point with newly created "Heap" variable

         Xrefs.Table (Xrefs.Last).Key.Ent := Heap;

         Nrefs         := Nrefs + 1;
         Rnums (Nrefs) := Xrefs.Last;
      end loop;

      --  Eliminate entries not appropriate for Alfa. Done prior to sorting
      --  cross-references, as it discards useless references which do not have
      --  a proper format for the comparison function (like no location).

      Eliminate_Before_Sort : declare
         NR : Nat;

         function Is_Alfa_Reference
           (E   : Entity_Id;
            Typ : Character) return Boolean;
         --  Return whether entity reference E meets Alfa requirements. Typ
         --  is the reference type.

         function Is_Alfa_Scope (E : Entity_Id) return Boolean;
         --  Return whether the entity or reference scope meets requirements
         --  for being an Alfa scope.

         function Is_Global_Constant (E : Entity_Id) return Boolean;
         --  Return True if E is a global constant for which we should ignore
         --  reads in Alfa.

         -----------------------
         -- Is_Alfa_Reference --
         -----------------------

         function Is_Alfa_Reference
           (E   : Entity_Id;
            Typ : Character) return Boolean
         is
         begin
            --  The only references of interest on callable entities are calls.
            --  On non-callable entities, the only references of interest are
            --  reads and writes.

            if Ekind (E) in Overloadable_Kind then
               return Typ = 's';

            --  References to constant objects are not considered in Alfa
            --  section, as these will be translated as constants in the
            --  intermediate language for formal verification, and should
            --  therefore never appear in frame conditions.

            elsif Is_Constant_Object (E) then
                  return False;

            --  Objects of Task type or protected type are not Alfa references

            elsif Present (Etype (E))
              and then Ekind (Etype (E)) in Concurrent_Kind
            then
               return False;

            --  In all other cases, result is true for reference/modify cases,
            --  and false for all other cases.

            else
               return Typ = 'r' or else Typ = 'm';
            end if;
         end Is_Alfa_Reference;

         -------------------
         -- Is_Alfa_Scope --
         -------------------

         function Is_Alfa_Scope (E : Entity_Id) return Boolean is
         begin
            return Present (E)
              and then not Is_Generic_Unit (E)
              and then Renamed_Entity (E) = Empty
              and then Get_Scope_Num (E) /= No_Scope;
         end Is_Alfa_Scope;

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
            if Alfa_Entities (Ekind (Xrefs.Table (Rnums (J)).Key.Ent))
              and then Alfa_References (Xrefs.Table (Rnums (J)).Key.Typ)
              and then Is_Alfa_Scope (Xrefs.Table (Rnums (J)).Key.Ent_Scope)
              and then Is_Alfa_Scope (Xrefs.Table (Rnums (J)).Key.Ref_Scope)
              and then not Is_Global_Constant (Xrefs.Table (Rnums (J)).Key.Ent)
              and then Is_Alfa_Reference (Xrefs.Table (Rnums (J)).Key.Ent,
                                          Xrefs.Table (Rnums (J)).Key.Typ)
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
            if Xrefs.Table (Rnums (J)).Key.Loc /= Crloc
              or else (Prevt = 'm'
                        and then Xrefs.Table (Rnums (J)).Key.Typ = 'r')
            then
               Crloc         := Xrefs.Table (Rnums (J)).Key.Loc;
               Prevt         := Xrefs.Table (Rnums (J)).Key.Typ;
               Nrefs         := Nrefs + 1;
               Rnums (Nrefs) := Rnums (J);
            end if;
         end loop;
      end Eliminate_After_Sort;

      --  Initialize loop

      Cur_Scope_Idx  := 1;
      From_Xref_Idx  := 1;
      Cur_Entity     := Empty;

      if Alfa_Scope_Table.Last = 0 then
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
            --  table Alfa_Scope_Table.

            function Get_Entity_Type (E : Entity_Id) return Character;
            --  Return a character representing the type of entity

            function Is_Future_Scope_Entity (E : Entity_Id) return Boolean;
            --  Check whether entity E is in Alfa_Scope_Table at index
            --  Cur_Scope_Idx or higher.

            function Is_Past_Scope_Entity (E : Entity_Id) return Boolean;
            --  Check whether entity E is in Alfa_Scope_Table at index strictly
            --  lower than Cur_Scope_Idx.

            ---------------
            -- Cur_Scope --
            ---------------

            function Cur_Scope return Node_Id is
            begin
               return Alfa_Scope_Table.Table (Cur_Scope_Idx).Scope_Entity;
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
               for J in Cur_Scope_Idx .. Alfa_Scope_Table.Last loop
                  if E = Alfa_Scope_Table.Table (J).Scope_Entity then
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
               for J in Alfa_Scope_Table.First .. Cur_Scope_Idx - 1 loop
                  if E = Alfa_Scope_Table.Table (J).Scope_Entity then
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
            --  not in Alfa scope table, which reveals either a problem in the
            --  construction of the scope table, or an erroneous scope for the
            --  current cross-reference.

            pragma Assert (Is_Future_Scope_Entity (XE.Key.Ent_Scope));

            --  Update the range of cross references to which the current scope
            --  refers to. This may be the empty range only for the first scope
            --  considered.

            if XE.Key.Ent_Scope /= Cur_Scope then
               Alfa_Scope_Table.Table (Cur_Scope_Idx).From_Xref :=
                 From_Xref_Idx;
               Alfa_Scope_Table.Table (Cur_Scope_Idx).To_Xref :=
                 Alfa_Xref_Table.Last;
               From_Xref_Idx := Alfa_Xref_Table.Last + 1;
            end if;

            while XE.Key.Ent_Scope /= Cur_Scope loop
               Cur_Scope_Idx := Cur_Scope_Idx + 1;
               pragma Assert (Cur_Scope_Idx <= Alfa_Scope_Table.Last);
            end loop;

            if XE.Key.Ent /= Cur_Entity then
               Cur_Entity_Name :=
                 new String'(Unique_Name (XE.Key.Ent));
            end if;

            if XE.Key.Ent = Heap then
               Alfa_Xref_Table.Append (
                 (Entity_Name => Cur_Entity_Name,
                  Entity_Line => 0,
                  Etype       => Get_Entity_Type (XE.Key.Ent),
                  Entity_Col  => 0,
                  File_Num    => Dependency_Num (XE.Key.Lun),
                  Scope_Num   => Get_Scope_Num (XE.Key.Ref_Scope),
                  Line        => Int (Get_Logical_Line_Number (XE.Key.Loc)),
                  Rtype       => XE.Key.Typ,
                  Col         => Int (Get_Column_Number (XE.Key.Loc))));

            else
               Alfa_Xref_Table.Append (
                 (Entity_Name => Cur_Entity_Name,
                  Entity_Line => Int (Get_Logical_Line_Number (XE.Def)),
                  Etype       => Get_Entity_Type (XE.Key.Ent),
                  Entity_Col  => Int (Get_Column_Number (XE.Def)),
                  File_Num    => Dependency_Num (XE.Key.Lun),
                  Scope_Num   => Get_Scope_Num (XE.Key.Ref_Scope),
                  Line        => Int (Get_Logical_Line_Number (XE.Key.Loc)),
                  Rtype       => XE.Key.Typ,
                  Col         => Int (Get_Column_Number (XE.Key.Loc))));
            end if;
         end Add_One_Xref;
      end loop;

      --  Update the range of cross references to which the scope refers to

      Alfa_Scope_Table.Table (Cur_Scope_Idx).From_Xref := From_Xref_Idx;
      Alfa_Scope_Table.Table (Cur_Scope_Idx).To_Xref   := Alfa_Xref_Table.Last;
   end Add_Alfa_Xrefs;

   ------------------
   -- Collect_Alfa --
   ------------------

   procedure Collect_Alfa (Sdep_Table : Unit_Ref_Table; Num_Sdep : Nat) is
   begin
      --  Cross-references should have been computed first

      pragma Assert (Xrefs.Last /= 0);

      Initialize_Alfa_Tables;

      --  Generate file and scope Alfa information

      for D in 1 .. Num_Sdep loop
         Add_Alfa_File (U => Sdep_Table (D), D => D);
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

         for S in Alfa_Scope_Table.First .. Alfa_Scope_Table.Last loop
            declare
               Srec : Alfa_Scope_Record renames Alfa_Scope_Table.Table (S);
            begin
               Entity_Hash_Table.Set (Srec.Scope_Entity, S);
            end;
         end loop;

         --  Use the hash-table to locate spec entities

         for S in Alfa_Scope_Table.First .. Alfa_Scope_Table.Last loop
            declare
               Srec : Alfa_Scope_Record renames Alfa_Scope_Table.Table (S);

               Spec_Entity : constant Entity_Id :=
                               Unique_Entity (Srec.Scope_Entity);
               Spec_Scope  : constant Scope_Index :=
                               Entity_Hash_Table.Get (Spec_Entity);

            begin
               --  Spec of generic may be missing, in which case Spec_Scope is
               --  zero.

               if Spec_Entity /= Srec.Scope_Entity
                 and then Spec_Scope /= 0
               then
                  Srec.Spec_File_Num :=
                    Alfa_Scope_Table.Table (Spec_Scope).File_Num;
                  Srec.Spec_Scope_Num :=
                    Alfa_Scope_Table.Table (Spec_Scope).Scope_Num;
               end if;
            end;
         end loop;
      end;

      --  Generate cross reference Alfa information

      Add_Alfa_Xrefs;
   end Collect_Alfa;

   -------------------------------
   -- Detect_And_Add_Alfa_Scope --
   -------------------------------

   procedure Detect_And_Add_Alfa_Scope (N : Node_Id) is
   begin
      if Nkind_In (N, N_Subprogram_Declaration,
                      N_Subprogram_Body,
                      N_Subprogram_Body_Stub,
                      N_Package_Declaration,
                      N_Package_Body)
      then
         Add_Alfa_Scope (N);
      end if;
   end Detect_And_Add_Alfa_Scope;

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

      loop
         exit when No (Result);

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

      --  Do no return a scope without a proper location

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
      Indx      : Nat;
      Ref       : Source_Ptr;
      Ref_Scope : Entity_Id;

   begin
      Ref := Original_Location (Sloc (N));

      if Ref > No_Location then
         Drefs.Increment_Last;
         Indx := Drefs.Last;

         Ref_Scope := Enclosing_Subprogram_Or_Package (N);

         --  Entity is filled later on with the special "Heap" variable

         Drefs.Table (Indx).Key.Ent := Empty;

         Drefs.Table (Indx).Def := No_Location;
         Drefs.Table (Indx).Key.Loc := Ref;
         Drefs.Table (Indx).Key.Typ := Typ;

         --  It is as if the special "Heap" was defined in every scope where it
         --  is referenced.

         Drefs.Table (Indx).Key.Eun := Get_Source_Unit (Ref);
         Drefs.Table (Indx).Key.Lun := Get_Source_Unit (Ref);

         Drefs.Table (Indx).Key.Ref_Scope := Ref_Scope;
         Drefs.Table (Indx).Key.Ent_Scope := Ref_Scope;
         Drefs.Table (Indx).Ent_Scope_File := Get_Source_Unit (Ref_Scope);
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

            --  Generic subprogram declaration ??? TBD

            when N_Generic_Subprogram_Declaration =>
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

end Alfa;
