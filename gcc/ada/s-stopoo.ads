------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . S T O R A G E _ P O O L S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-1999 Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
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

with Ada.Finalization;
with System.Storage_Elements;

package System.Storage_Pools is
   pragma Preelaborate (System.Storage_Pools);

   type Root_Storage_Pool is abstract
     new Ada.Finalization.Limited_Controlled with private;

   procedure Allocate
     (Pool                     : in out Root_Storage_Pool;
      Storage_Address          : out Address;
      Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
      Alignment                : in System.Storage_Elements.Storage_Count)
   is abstract;

   procedure Deallocate
     (Pool                     : in out Root_Storage_Pool;
      Storage_Address          : in Address;
      Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
      Alignment                : in System.Storage_Elements.Storage_Count)
   is abstract;

   function Storage_Size
     (Pool : Root_Storage_Pool)
      return System.Storage_Elements.Storage_Count
   is abstract;

private
   type Root_Storage_Pool is abstract
     new Ada.Finalization.Limited_Controlled with null record;
end System.Storage_Pools;
