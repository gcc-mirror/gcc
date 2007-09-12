------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S T O R A G E _ E L E M E N T S              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2002-2007, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the implementation dependent sections of this file.      --
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

--  Warning: declarations in this package are ambiguous with respect to the
--  extra declarations that can be introduced into System using Extend_System.
--  It is a good idea to avoid use clauses for this package!

pragma Warnings (Off);
pragma Compiler_Unit;
pragma Warnings (On);

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

   pragma Warnings (Off);
   pragma Universal_Aliasing (Storage_Element);
   pragma Warnings (On);
   --  This type is used by the expansion to implement aggregate copy.
   --  We turn off warnings for this pragma to deal with being compiled
   --  with an earlier GNAT version that does not recognize this pragma.

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

   --  The following is a dummy record designed to mimic Communication_Block as
   --  defined in s-tpobop.ads:

   --     type Communication_Block is record
   --        Self      : Task_Id;  --  An access type
   --        Enqueued  : Boolean := True;
   --        Cancelled : Boolean := False;
   --     end record;

   --  The record is used in the construction of the predefined dispatching
   --  primitive _disp_asynchronous_select in order to avoid the import of
   --  System.Tasking.Protected_Objects.Operations. Note that this package
   --  is always imported in the presence of interfaces since the dispatch
   --  table uses entities from here.

   type Dummy_Communication_Block is record
      Comp_1 : Address;  --  Address and access have the same size
      Comp_2 : Boolean;
      Comp_3 : Boolean;
   end record;

end System.Storage_Elements;
