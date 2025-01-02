------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
-- S Y S T E M . A D D R E S S _ T O _ A C C E S S _ C O N V E R S I O N S  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Provides the means to convert addresses to access types as defined by ARM
--  13.7.2.

generic
   type Object (<>) is limited private;

package System.Address_To_Access_Conversions is
   pragma Preelaborate;

   pragma Compile_Time_Warning
     (Object'Unconstrained_Array,
      "Object is unconstrained array type" & ASCII.LF &
      "To_Pointer results may not have bounds");

   type Object_Pointer is access all Object;
   for Object_Pointer'Size use Standard'Address_Size;
   --  Access type definition to the object

   pragma No_Strict_Aliasing (Object_Pointer);
   --  Strictly speaking, this routine should not be used to generate pointers
   --  to other than proper values of the proper type, but in practice, this
   --  is done all the time. This pragma stops the compiler from doing some
   --  optimizations that may cause unexpected results based on the assumption
   --  of no strict aliasing.

   function To_Pointer (Value : Address)        return Object_Pointer with
     Global => null;
   --  Return ``Value`` as an access to *Object*.
   --
   --  This function is an intrinsic so implemented by the compiler.

   function To_Address (Value : Object_Pointer) return Address with
     SPARK_Mode => Off;
   --  Return ``Value`` as an *Address*.
   --
   --  This function is an intrinsic so implemented by the compiler.

   pragma Import (Intrinsic, To_Pointer);
   pragma Import (Intrinsic, To_Address);

end System.Address_To_Access_Conversions;
