------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . S T O R A G E _ P O O L S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2002 Free Software Foundation, Inc.            --
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

package body System.Storage_Pools is

   ------------------
   -- Allocate_Any --
   ------------------

   procedure Allocate_Any
    (Pool                     : in out Root_Storage_Pool'Class;
     Storage_Address          : out Address;
     Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
     Alignment                : System.Storage_Elements.Storage_Count)
   is
   begin
      Allocate
        (Pool, Storage_Address, Size_In_Storage_Elements, Alignment);
   end Allocate_Any;

   --------------------
   -- Deallocate_Any --
   --------------------

   procedure Deallocate_Any
    (Pool                     : in out Root_Storage_Pool'Class;
     Storage_Address          : Address;
     Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
     Alignment                : System.Storage_Elements.Storage_Count)
   is
   begin
      Deallocate
        (Pool, Storage_Address, Size_In_Storage_Elements, Alignment);
   end Deallocate_Any;
end System.Storage_Pools;
