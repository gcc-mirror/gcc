------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--         S Y S T E M . T A S K I N G . T A S K _ A T T R I B U T E S      --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--            Copyright (C) 2014-2025, Free Software Foundation, Inc.       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides support for the body of Ada.Task_Attributes

with Ada.Unchecked_Conversion;

package System.Tasking.Task_Attributes is

   type Deallocator is access procedure (Ptr : System.Address);
   pragma Favor_Top_Level (Deallocator);

   type Attribute_Record is record
      Free : Deallocator;
   end record;
   --  The real type is declared in Ada.Task_Attributes body: Real_Attribute.
   --  As long as the first field is the deallocator we are good.

   type Attribute_Access is access all Attribute_Record;
   pragma No_Strict_Aliasing (Attribute_Access);

   function To_Attribute is new
     Ada.Unchecked_Conversion (System.Address, Attribute_Access);

   function Next_Index (Require_Finalization : Boolean) return Integer;
   --  Return the next attribute index available. Require_Finalization is True
   --  if the attribute requires finalization and in particular its deallocator
   --  (Free field in Attribute_Record) should be called. Raise Storage_Error
   --  if no index is available.

   function Require_Finalization (Index : Integer) return Boolean;
   --  Return True if a given attribute index requires call to Free. This call
   --  is not protected against concurrent access, should only be called during
   --  finalization of the corresponding instantiation of Ada.Task_Attributes,
   --  or during finalization of a task.

   procedure Finalize (Index : Integer);
   --  Finalize given Index, possibly allowing future reuse

private
   pragma Inline (Finalize);
   pragma Inline (Require_Finalization);
end System.Tasking.Task_Attributes;
