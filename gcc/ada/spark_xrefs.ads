------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           S P A R K _ X R E F S                          --
--                                                                          --
--                                 S p e c                                  --
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

--  This package defines tables used to store information needed for the SPARK
--  mode. It is used by procedures in Lib.Xref.SPARK_Specific to build the
--  SPARK-specific cross-reference information.

with Table;
with Types; use Types;

package SPARK_Xrefs is

   --  SPARK cross-reference information is stored internally using three
   --  tables: SPARK_Xref_Table, SPARK_Scope_Table and SPARK_File_Table, which
   --  are defined in this unit.

   --  Lib.Xref.SPARK_Specific is part of the compiler. It extracts SPARK
   --  cross-reference information from the complete set of cross-references
   --  generated during compilation.

   --  -------------------------------
   --  -- Generated Globals Section --
   --  -------------------------------

   --  The Generated Globals section is located at the end of the ALI file

   --  All lines with information related to the Generated Globals begin with
   --  string "GG". This string should therefore not be used in the beginning
   --  of any line not related to Generated Globals.

   --  The processing (reading and writing) of this section happens in package
   --  Flow_Generated_Globals (from the SPARK 2014 sources), for further
   --  information please refer there.

   ----------------
   -- Xref Table --
   ----------------

   --  The following table records SPARK cross-references

   type Xref_Index is new Nat;
   --  Used to index values in this table. Values start at 1 and are assigned
   --  sequentially as entries are constructed; value 0 is used temporarily
   --  until a proper value is determined.

   type SPARK_Xref_Record is record
      Entity : Entity_Id;
      --  Referenced entity

      Ref_Scope : Entity_Id;
      --  Scope where the reference occurs

      Rtype : Character;
      --  Indicates type of the reference, using code used in ALI file:
      --    r = reference
      --    m = modification
      --    s = call
   end record;

   package SPARK_Xref_Table is new Table.Table (
     Table_Component_Type => SPARK_Xref_Record,
     Table_Index_Type     => Xref_Index,
     Table_Low_Bound      => 1,
     Table_Initial        => 2000,
     Table_Increment      => 300,
     Table_Name           => "Xref_Table");

   -----------------
   -- Scope Table --
   -----------------

   --  This table keeps track of the scopes and the corresponding starting and
   --  ending indexes (From, To) in the Xref table.

   type Scope_Index is new Nat;
   --  Used to index values in this table. Values start at 1 and are assigned
   --  sequentially as entries are constructed; value 0 indicates that no
   --  entries have been constructed and is also used until a proper value is
   --  determined.

   type SPARK_Scope_Record is record
      Entity : Entity_Id;
      --  Entity that is represented by the scope

      Scope_Num : Pos;
      --  Set to the scope number within the enclosing unit

      From_Xref : Xref_Index;
      --  Starting index in Xref table for this scope

      To_Xref : Xref_Index;
      --  Ending index in Xref table for this scope
   end record;

   package SPARK_Scope_Table is new Table.Table (
     Table_Component_Type => SPARK_Scope_Record,
     Table_Index_Type     => Scope_Index,
     Table_Low_Bound      => 1,
     Table_Initial        => 200,
     Table_Increment      => 300,
     Table_Name           => "Scope_Table");

   ----------------
   -- File Table --
   ----------------

   --  This table keeps track of the units and the corresponding starting and
   --  ending indexes (From, To) in the Scope table.

   type File_Index is new Nat;
   --  Used to index values in this table. Values start at 1 and are assigned
   --  sequentially as entries are constructed; value 0 indicates that no
   --  entries have been constructed.

   type SPARK_File_Record is record
      File_Num : Nat;
      --  Dependency number in ALI file

      From_Scope : Scope_Index;
      --  Starting index in Scope table for this unit

      To_Scope : Scope_Index;
      --  Ending index in Scope table for this unit
   end record;

   package SPARK_File_Table is new Table.Table (
     Table_Component_Type => SPARK_File_Record,
     Table_Index_Type     => File_Index,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 200,
     Table_Name           => "File_Table");

   ---------------
   -- Constants --
   ---------------

   Name_Of_Heap_Variable : constant String := "__HEAP";
   --  Name of special variable used in effects to denote reads and writes
   --  through explicit dereference.

   Heap : Entity_Id := Empty;
   --  A special entity which denotes the heap object; it should be considered
   --  constant, but needs to be variable, because it can only be initialized
   --  after the node tables are created.

   -----------------
   -- Subprograms --
   -----------------

   procedure Initialize_SPARK_Tables;
   --  Reset tables for a new compilation

   procedure dspark;
   --  Debug routine to dump internal SPARK cross-reference tables. This is a
   --  raw format dump showing exactly what the tables contain.

end SPARK_Xrefs;
