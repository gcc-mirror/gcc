------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S T O R A G E _ E L E M E N T S              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2002-2020, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the implementation dependent sections of this file.      --
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

--  Warning: declarations in this package are ambiguous with respect to the
--  extra declarations that can be introduced into System using Extend_System.
--  It is a good idea to avoid use clauses for this package.

pragma Compiler_Unit_Warning;

package System.Storage_Elements is
   pragma Pure;
   --  Note that we take advantage of the implementation permission to make
   --  this unit Pure instead of Preelaborable; see RM 13.7.1(15). In Ada 2005,
   --  this is Pure in any case (AI-362).

   --  We also add the pragma Pure_Function to the operations in this package,
   --  because otherwise functions with parameters derived from Address are
   --  treated as non-pure by the back-end (see exp_ch6.adb). This is because
   --  in many cases such a parameter is used to hide read/out access to
   --  objects, and it would be unsafe to treat such functions as pure.

   type Storage_Offset is range
     -(2 ** (Integer'(Standard'Address_Size) - 1)) ..
     +(2 ** (Integer'(Standard'Address_Size) - 1)) - Long_Long_Integer'(1);
   --  Note: the reason for the Long_Long_Integer qualification here is to
   --  avoid a bogus ambiguity when this unit is analyzed in an rtsfind
   --  context. It may be possible to remove this in the future, but it is
   --  certainly harmless in any case ???

   subtype Storage_Count is Storage_Offset range 0 .. Storage_Offset'Last;

   type Storage_Element is mod 2 ** Storage_Unit;
   for Storage_Element'Size use Storage_Unit;

   pragma Universal_Aliasing (Storage_Element);
   --  This type is used by the expander to implement aggregate copy

   type Storage_Array is
     array (Storage_Offset range <>) of aliased Storage_Element;
   for Storage_Array'Component_Size use Storage_Unit;

   --  Address arithmetic

   function "+" (Left : Address; Right : Storage_Offset) return Address;
   pragma Convention (Intrinsic, "+");
   pragma Inline_Always ("+");
   pragma Pure_Function ("+");

   function "+" (Left : Storage_Offset; Right : Address) return Address;
   pragma Convention (Intrinsic, "+");
   pragma Inline_Always ("+");
   pragma Pure_Function ("+");

   function "-" (Left : Address; Right : Storage_Offset) return Address;
   pragma Convention (Intrinsic, "-");
   pragma Inline_Always ("-");
   pragma Pure_Function ("-");

   function "-" (Left, Right : Address) return Storage_Offset;
   pragma Convention (Intrinsic, "-");
   pragma Inline_Always ("-");
   pragma Pure_Function ("-");

   function "mod"
     (Left  : Address;
      Right : Storage_Offset) return  Storage_Offset;
   pragma Convention (Intrinsic, "mod");
   pragma Inline_Always ("mod");
   pragma Pure_Function ("mod");

   --  Conversion to/from integers

   type Integer_Address is mod Memory_Size;

   function To_Address (Value : Integer_Address) return Address;
   pragma Convention (Intrinsic, To_Address);
   pragma Inline_Always (To_Address);
   pragma Pure_Function (To_Address);

   function To_Integer (Value : Address) return Integer_Address;
   pragma Convention (Intrinsic, To_Integer);
   pragma Inline_Always (To_Integer);
   pragma Pure_Function (To_Integer);

end System.Storage_Elements;
