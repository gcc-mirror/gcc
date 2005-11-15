------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ M A P S                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1996-2005, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the operations on the renaming maps used for
--  generic analysis and instantiation. Renaming maps are created when
--  a generic unit is analyzed, in order to capture all references to
--  global variables within the unit. The renaming map of a generic unit
--  copied prior to each instantiation, and then updated by mapping the
--  formals into the actuals and the local entities into entities local to
--  the instance. When the generic tree is copied to produce the instance,
--  all references are updated by means of the renaming map.

--  Map composition of renaming maps takes place for nested instantiations,
--  for generic child units, and for formal packages.

--  For additional details, see the documentation in sem_ch12

with Table;
with Types; use Types;

package Sem_Maps is

   type Map is new Int;

   type Assoc is private;

   type Scope_Kind is (S_Global, S_Formal, S_Local);

   function New_Map (Num_Assoc : Int) return Map;
   --  Build empty map with the given number of associations, and a
   --  headers table of the appropriate size.

   function Compose (Orig_Map : Map; New_Map : Map) return Map;
   --  Update the associations in Orig_Map, so that if Orig_Map (e1) = e2
   --  and New_Map (e2) = e3, then the image of e1 under the result is e3.

   function Copy (M : Map) return Map;
   --  Full copy of contents and headers

   function Lookup (M : Map; E : Entity_Id) return Entity_Id;
   --  Retrieve image of E under M, Empty if undefined

   procedure Add_Association
     (M    : in out Map;
      O_Id : Entity_Id;
      N_Id : Entity_Id;
      Kind : Scope_Kind := S_Local);
   --  Update M in place. On entry M (O_Id) must not be defined

   procedure Update_Association
     (M    : in out Map;
      O_Id : Entity_Id;
      N_Id : Entity_Id;
      Kind : Scope_Kind := S_Local);
   --  Update the entry in M for O_Id

   function Build_Instance_Map (M : Map) return Map;
   --  Copy renaming map of generic, and create new entities for all the
   --  local entities within.

private

   --  New maps are created when a generic is analyzed, and for each of
   --  its instantiations. Maps are also updated for nested generics, for
   --  child units, and for formal packages. As a result we need to allocate
   --  maps dynamically.

   --  When analyzing a generic, we do not know how many references are
   --  in it. We build an initial map after generic analysis, using a static
   --  structure that relies on the compiler's extensible table mechanism.
   --  After constructing this initial map, all subsequent uses and updates
   --  of this map do not modify its domain, so that dynamically allocated
   --  maps have a fixed size and never need to be reallocated. Furthermore,
   --  the headers of the hash table of a dynamically allocated map can be
   --  chosen according to the total number of entries in the map, to
   --  accommodate efficiently generic units of different sizes (Unchecked_
   --  Conversion vs. Generic_Elementary_Functions, for example). So in
   --  fact both components of a map have fixed size, and can be allocated
   --  using the standard table mechanism. A Maps_Table holds records that
   --  contain indices into the global Headers table and the Associations
   --  table, and a Map is an index into the Maps_Table.
   --
   --              Maps_Table          Headers_Table     Associations_Table
   --
   --                                    |_____|          |___________ |
   --               |_____|              |     |          |            |
   --        ------>|Map  |------------------------------>|Associations|
   --               |Info |------------->|     |=========>| for one    |
   --               |_____|              |     |====|     |   unit     |
   --               |     |              |     |    |====>|            |
   --                                    |_____|          |____________|
   --                                    |     |          |            |
   type Header_Index is new Int;
   type Assoc_Index  is new Int;
   No_Assoc : constant Assoc_Index := -1;

   type Map_Info is record
      Header_Offset : Header_Index;
      Header_Num    : Header_Index;
      Assoc_Offset  : Assoc_Index;
      Assoc_Num     : Assoc_Index;
      Assoc_Next    : Assoc_Index;
   end record;

   type Assoc is record
      Old_Id : Entity_Id   := Empty;
      New_Id : Entity_Id   := Empty;
      Kind   : Scope_Kind  := S_Local;
      Next   : Assoc_Index := No_Assoc;
   end record;

   --  All maps are accessed through the following table. The map attribute
   --  of a generic unit or an instance is an index into this table.

   package Maps_Table is new Table.Table (
      Table_Component_Type => Map_Info,
      Table_Index_Type     => Map,
      Table_Low_Bound      => 0,
      Table_Initial        => 100,
      Table_Increment      => 10,
      Table_Name           => "Maps_Table");

   --  All headers for hash tables are allocated in one global table. Each
   --  map stores the offset into this table at which its own headers start.

   package Headers_Table is new Table.Table (
      Table_Component_Type => Assoc_Index,
      Table_Index_Type     => Header_Index,
      Table_Low_Bound      => 0,
      Table_Initial        => 1000,
      Table_Increment      => 10,
      Table_Name           => "Headers_Table");

   --  All associations are allocated in one global table. Each map stores
   --  the offset into this table at which its own associations start.

   package Associations_Table is new Table.Table (
      Table_Component_Type => Assoc,
      Table_Index_Type     => Assoc_Index,
      Table_Low_Bound      => 1,
      Table_Initial        => 1000,
      Table_Increment      => 10,
      Table_Name           => "Associations_Table");

end Sem_Maps;
