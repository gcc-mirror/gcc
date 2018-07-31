------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--              L I B . X R E F . S P A R K _ S P E C I F I C               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2011-2018, Free Software Foundation, Inc.         --
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

   -------------------------
   -- Iterate_SPARK_Xrefs --
   -------------------------

   procedure Iterate_SPARK_Xrefs is

      procedure Add_SPARK_Xref (Index : Int; Xref : Xref_Entry);

      function Is_SPARK_Reference
        (E   : Entity_Id;
         Typ : Character) return Boolean;
      --  Return whether entity reference E meets SPARK requirements. Typ is
      --  the reference type.

      function Is_SPARK_Scope (E : Entity_Id) return Boolean;
      --  Return whether the entity or reference scope meets requirements for
      --  being a SPARK scope.

      --------------------
      -- Add_SPARK_Xref --
      --------------------

      procedure Add_SPARK_Xref (Index : Int; Xref : Xref_Entry) is
         Ref : Xref_Key renames Xref.Key;
      begin
         --  Eliminate entries not appropriate for SPARK

         if SPARK_Entities (Ekind (Ref.Ent))
           and then SPARK_References (Ref.Typ)
           and then Is_SPARK_Scope (Ref.Ent_Scope)
           and then Is_SPARK_Scope (Ref.Ref_Scope)
           and then Is_SPARK_Reference (Ref.Ent, Ref.Typ)
         then
            Process
              (Index,
               (Entity    => Ref.Ent,
                Ref_Scope => Ref.Ref_Scope,
                Rtype     => Ref.Typ));
         end if;

      end Add_SPARK_Xref;

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
           and then (not Can_Be_Renamed or else No (Renamed_Entity (E)));
      end Is_SPARK_Scope;

   --  Start of processing for Add_SPARK_Xrefs

   begin
      --  Expose cross-references from private frontend tables to the backend

      for Index in Drefs.First .. Drefs.Last loop
         Add_SPARK_Xref (Index, Drefs.Table (Index));
      end loop;

      for Index in Xrefs.First .. Xrefs.Last loop
         Add_SPARK_Xref (-Index, Xrefs.Table (Index));
      end loop;
   end Iterate_SPARK_Xrefs;

   ---------------------------------------------
   -- Enclosing_Subprogram_Or_Library_Package --
   ---------------------------------------------

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
         Set_Scope       (Heap, Standard_Standard);
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

end SPARK_Specific;
