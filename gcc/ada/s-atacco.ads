------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
-- S Y S T E M . A D D R E S S _ T O _ A C C E S S _ C O N V E R S I O N S  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
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

generic
   type Object (<>) is limited private;

package System.Address_To_Access_Conversions is
pragma Preelaborate (Address_To_Access_Conversions);

pragma Elaborate_Body;
--  This pragma Elaborate_Body is there to ensure the requirement of what is
--  at the moment a dummy null body. The reason this null body is there is
--  that we used to have a real body, and it causes bootstrap problems with
--  old compilers if we try to remove the corresponding file.

   pragma Compile_Time_Warning
     (Object'Unconstrained_Array,
      "Object is unconstrained array type" & ASCII.LF &
      "To_Pointer results may not have bounds");

   --  Capture constrained status, suppressing warnings, since this is
   --  an obsolescent feature to use Constrained in this way (RM J.4).

   pragma Warnings (Off);
   Xyz : Boolean := Object'Constrained;
   pragma Warnings (On);

   type Object_Pointer is access all Object;
   for Object_Pointer'Size use Standard'Address_Size;

   function To_Pointer (Value : Address)        return Object_Pointer;
   function To_Address (Value : Object_Pointer) return Address;

   pragma Import (Intrinsic, To_Pointer);
   pragma Import (Intrinsic, To_Address);

end System.Address_To_Access_Conversions;
