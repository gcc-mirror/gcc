------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S T O R A G E _ E L E M E N T S              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  Warning: declarations in this package are ambiguous with respect to the
--  extra declarations that can be introduced into System using Extend_System.
--  It is a good idea to avoid use clauses for this package!

pragma Warnings (Off);
--  This is to stop bootstrap problems with the use of Inline_Always
--  To be removed (along with redundant Inline pragmas) in 3.13.

package System.Storage_Elements is
pragma Pure (Storage_Elements);
--  Note that we take advantage of the implementation permission to make
--  this unit Pure instead of Preelaborable; see RM 13.7.1(15).

   type Storage_Offset is range
     -(2 ** (Standard."-" (Standard'Address_Size, 1))) ..
     +(2 ** (Standard."-" (Standard'Address_Size, 1))) - 1;
   --  Note: the reason for the qualification of "-" here by Standard is
   --  that we have a current bug in GNAT that otherwise causes a bogus
   --  ambiguity when this unit is analyzed in an Rtsfind context.

   subtype Storage_Count is Storage_Offset range 0 .. Storage_Offset'Last;

   type Storage_Element is mod 2 ** Storage_Unit;
   for Storage_Element'Size use Storage_Unit;

   type Storage_Array is
     array (Storage_Offset range <>) of aliased Storage_Element;
   for Storage_Array'Component_Size use Storage_Unit;

   --  Address arithmetic

   function "+" (Left : Address; Right : Storage_Offset) return Address;
   pragma Convention (Intrinsic, "+");
   pragma Inline ("+");
   pragma Inline_Always ("+");

   function "+" (Left : Storage_Offset; Right : Address) return Address;
   pragma Convention (Intrinsic, "+");
   pragma Inline ("+");
   pragma Inline_Always ("+");

   function "-" (Left : Address; Right : Storage_Offset) return Address;
   pragma Convention (Intrinsic, "-");
   pragma Inline ("-");
   pragma Inline_Always ("-");

   function "-" (Left, Right : Address) return Storage_Offset;
   pragma Convention (Intrinsic, "-");
   pragma Inline ("-");
   pragma Inline_Always ("-");

   function "mod"
     (Left  : Address;
      Right : Storage_Offset)
      return  Storage_Offset;
   pragma Convention (Intrinsic, "mod");
   pragma Inline ("mod");
   pragma Inline_Always ("mod");

   --  Conversion to/from integers

   type Integer_Address is mod Memory_Size;

   function To_Address (Value : Integer_Address) return Address;
   pragma Convention (Intrinsic, To_Address);
   pragma Inline (To_Address);
   pragma Inline_Always (To_Address);

   function To_Integer (Value : Address) return Integer_Address;
   pragma Convention (Intrinsic, To_Integer);
   pragma Inline (To_Integer);
   pragma Inline_Always (To_Integer);

end System.Storage_Elements;
