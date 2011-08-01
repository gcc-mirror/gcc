------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    G N A T . B Y T E _ S W A P P I N G                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2006-2010, AdaCore                     --
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

--  Simple routines for swapping the bytes of 16-, 32-, and 64-bit objects

--  The generic functions should be instantiated with types that are of a size
--  in bytes corresponding to the name of the generic. For example, a 2-byte
--  integer type would be compatible with Swapped2, 4-byte integer with
--  Swapped4, and so on. Failure to do so will result in a warning when
--  compiling the instantiation; this warning should be heeded. Ignoring this
--  warning can result in unexpected results.

--  An example of proper usage follows:

--     declare
--        type Short_Integer is range -32768 .. 32767;
--        for Short_Integer'Size use 16; -- for confirmation

--        X : Short_Integer := 16#7FFF#;

--        function Swapped is new Byte_Swapping.Swapped2 (Short_Integer);

--     begin
--        Put_Line (X'Img);
--        X := Swapped (X);
--        Put_Line (X'Img);
--     end;

--  Note that the generic actual types need not be scalars, but must be
--  'definite' types. They can, for example, be constrained subtypes of
--  unconstrained array types as long as the size is correct. For instance,
--  a subtype of String with length of 4 would be compatible with the
--  Swapped4 generic:

--     declare
--        subtype String4 is String (1 .. 4);
--        function Swapped is new Byte_Swapping.Swapped4 (String4);
--        S : String4 := "ABCD";
--     begin
--        Put_Line (S);
--        S := Swapped (S);
--        Put_Line (S);
--     end;

--  Similarly, a constrained array type is also acceptable:

--     declare
--        type Mask is array (0 .. 15) of Boolean;
--        for Mask'Component_Size use Boolean'Size;
--        X : Mask := (0 .. 7 => True, others => False);
--        function Swapped is new Byte_Swapping.Swapped2 (Mask);
--     begin
--        ...
--        X := Swapped (X);
--        ...
--     end;

--  A properly-sized record type will also be acceptable, and so forth

--  However, as described, a size mismatch must be avoided. In the following we
--  instantiate one of the generics with a type that is too large. The result
--  of the function call is undefined, such that assignment to an object can
--  result in garbage values.

--     Wrong: declare
--        subtype String16 is String (1 .. 16);

--        function Swapped is new Byte_Swapping.Swapped8 (String16);
--        --  Instantiation generates a compiler warning about
--        --  mismatched sizes

--        S : String16;

--     begin
--        S := "ABCDEFGHDEADBEEF";
--
--        Put_Line (S);
--
--        --  the following assignment results in garbage in S after the
--        --  first 8 bytes
--
--        S := Swapped (S);
--
--        Put_Line (S);
--     end Wrong;

--  When the size of the type is larger than 8 bytes, the use of the non-
--  generic procedures is an alternative because no function result is
--  involved; manipulation of the object is direct.

--  The procedures are passed the address of an object to manipulate. They will
--  swap the first N bytes of that object corresponding to the name of the
--  procedure.  For example:

--     declare
--        S2 : String := "AB";
--        for S2'Alignment use 2;
--        S4 : String := "ABCD";
--        for S4'Alignment use 4;
--        S8 : String := "ABCDEFGH";
--        for S8'Alignment use 8;

--     begin
--        Swap2 (S2'Address);
--        Put_Line (S2);

--        Swap4 (S4'Address);
--        Put_Line (S4);

--        Swap8 (S8'Address);
--        Put_Line (S8);
--     end;

--  If an object of a type larger than N is passed, the remaining bytes of the
--  object are undisturbed. For example:

--     declare
--        subtype String16 is String (1 .. 16);

--        S : String16;
--        for S'Alignment use 8;

--     begin
--        S  := "ABCDEFGHDEADBEEF";
--        Put_Line (S);
--        Swap8 (S'Address);
--        Put_Line (S);
--     end;

with System;

package GNAT.Byte_Swapping is
   pragma Pure;

   --  NB: all the routines in this package treat the application objects as
   --  unsigned (modular) types of a size in bytes corresponding to the routine
   --  name. For example, the generic function Swapped2 manipulates the object
   --  passed to the formal parameter Input as a value of an unsigned type that
   --  is 2 bytes long. Therefore clients are responsible for the compatibility
   --  of application types manipulated by these routines and these modular
   --  types, in terms of both size and alignment. This requirement applies to
   --  the generic actual type passed to the generic formal type Item in the
   --  generic functions, as well as to the type of the object implicitly
   --  designated by the address passed to the non-generic procedures. Use of
   --  incompatible types can result in implementation- defined effects.

   generic
      type Item is limited private;
   function Swapped2 (Input : Item) return Item;
   --  Return the 2-byte value of Input with the bytes swapped

   generic
      type Item is limited private;
   function Swapped4 (Input : Item) return Item;
   --  Return the 4-byte value of Input with the bytes swapped

   generic
      type Item is limited private;
   function Swapped8 (Input : Item) return Item;
   --  Return the 8-byte value of Input with the bytes swapped

   procedure Swap2 (Location : System.Address);
   --  Swap the first 2 bytes of the object starting at the address specified
   --  by Location.

   procedure Swap4 (Location : System.Address);
   --  Swap the first 4 bytes of the object starting at the address specified
   --  by Location.

   procedure Swap8 (Location : System.Address);
   --  Swap the first 8 bytes of the object starting at the address specified
   --  by Location.

   pragma Inline (Swap2, Swap4, Swap8, Swapped2, Swapped4, Swapped8);

end GNAT.Byte_Swapping;
