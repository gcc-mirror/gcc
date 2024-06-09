------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           S P A R K _ X R E F S                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2011-2024, Free Software Foundation, Inc.         --
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

--  This package defines data structures used to expose frontend
--  cross-references to the SPARK backend.

with Types; use Types;

package SPARK_Xrefs is

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
   --  This type holds a subset of the frontend xref entry that is needed by
   --  the SPARK backend.

   ---------------
   -- Constants --
   ---------------

   Name_Of_Heap_Variable : constant String := "__HEAP";
   --  Name of special variable used in effects to denote reads and writes
   --  through explicit dereference.

   Heap : Entity_Id := Empty;
   --  A special entity which denotes the heap object; it should be considered
   --  constant, but needs to be variable, because it can only be initialized
   --  after the node tables are created. Also, it is only created if there is
   --  an actual need for it, and remains Empty otherwise.

   -----------------
   -- Subprograms --
   -----------------

   procedure dspark;
   --  Debug routine to dump internal SPARK cross-reference tables. This is a
   --  raw format dump showing exactly what the tables contain.

end SPARK_Xrefs;
