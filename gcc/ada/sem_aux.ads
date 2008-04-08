------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ A U X                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2008, Free Software Foundation, Inc.         --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Package containing utility procedures used throughout the compiler,
--  and also by ASIS so dependencies are limited to ASIS included packages.

--  Note: contents are minimal for now, the intent is to move stuff from
--  Sem_Util that meets the ASIS dependency requirements, and also stuff
--  from Einfo, where Einfo had excessive semantic knowledge of the tree.

with Alloc;   use Alloc;
with Table;
with Types;   use Types;

package Sem_Aux is

   --------------------------------
   -- Obsolescent Warnings Table --
   --------------------------------

   --  This table records entities for which a pragma Obsolescent with a
   --  message argument has been processed.

   type OWT_Record is record
      Ent : Entity_Id;
      --  The entity to which the pragma applies

      Msg : String_Id;
      --  The string containing the message
   end record;

   package Obsolescent_Warnings is new Table.Table (
     Table_Component_Type => OWT_Record,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.Obsolescent_Warnings_Initial,
     Table_Increment      => Alloc.Obsolescent_Warnings_Increment,
     Table_Name           => "Obsolescent_Warnings");

   -----------------
   -- Subprograms --
   -----------------

   procedure Initialize;
   --  Called at the start of compilation of each new main source file to
   --  initialize the allocation of the Obsolescent_Warnings table. Note that
   --  Initialize must not be called if Tree_Read is used.

   procedure Tree_Read;
   --  Initializes internal tables from current tree file using the relevant
   --  Table.Tree_Read routines.

   procedure Tree_Write;
   --  Writes out internal tables to current tree file using the relevant
   --  Table.Tree_Write routines.

end Sem_Aux;
