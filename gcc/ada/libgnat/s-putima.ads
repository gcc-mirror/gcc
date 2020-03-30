------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                             SYSTEM.PUT_IMAGES                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2020, Free Software Foundation, Inc.            --
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

pragma Ada_2020;
with Ada.Strings.Text_Output;
with System.Unsigned_Types;
package System.Put_Images is

   --  This package contains subprograms that are called by the generated code
   --  for the 'Put_Image attribute.
   --
   --  For an integer type that fits in Integer, the actual parameter is
   --  converted to Integer, and Put_Image_Integer is called. For larger types,
   --  Put_Image_Long_Long_Integer is used. Other numeric types are treated
   --  similarly. Access values are unchecked-converted to either Thin_Pointer
   --  or Fat_Pointer, and Put_Image_Thin_Pointer or Put_Image_Fat_Pointer is
   --  called. The Before/Between/After procedures are called before printing
   --  the components of a composite type, between pairs of components, and
   --  after them. See Exp_Put_Image in the compiler for details of these
   --  calls.

   subtype Sink is Ada.Strings.Text_Output.Sink;

   procedure Put_Image_Integer (S : in out Sink'Class; X : Integer);
   procedure Put_Image_Long_Long_Integer
     (S : in out Sink'Class; X : Long_Long_Integer);

   subtype Unsigned is System.Unsigned_Types.Unsigned;
   subtype Long_Long_Unsigned is System.Unsigned_Types.Long_Long_Unsigned;

   procedure Put_Image_Unsigned (S : in out Sink'Class; X : Unsigned);
   procedure Put_Image_Long_Long_Unsigned
     (S : in out Sink'Class; X : Long_Long_Unsigned);

   type Byte is new Character with Alignment => 1;
   type Byte_String is array (Positive range <>) of Byte with Alignment => 1;
   type Thin_Pointer is access all Byte;
   type Fat_Pointer is access all Byte_String;
   procedure Put_Image_Thin_Pointer (S : in out Sink'Class; X : Thin_Pointer);
   procedure Put_Image_Fat_Pointer (S : in out Sink'Class; X : Fat_Pointer);
   --  Print "null", or the address of the designated object as an unsigned
   --  hexadecimal integer.

   procedure Put_Image_Access_Subp (S : in out Sink'Class; X : Thin_Pointer);
   --  For access-to-subprogram types

   procedure Put_Image_Access_Prot_Subp
     (S : in out Sink'Class; X : Thin_Pointer);
   --  For access-to-protected-subprogram types

   procedure Put_Image_String (S : in out Sink'Class; X : String);
   procedure Put_Image_Wide_String (S : in out Sink'Class; X : Wide_String);
   procedure Put_Image_Wide_Wide_String
     (S : in out Sink'Class; X : Wide_Wide_String);

   procedure Array_Before (S : in out Sink'Class);
   procedure Array_Between (S : in out Sink'Class);
   procedure Array_After (S : in out Sink'Class);

   procedure Simple_Array_Between (S : in out Sink'Class);
   --  For "simple" arrays, where we don't want a newline between every
   --  component.

   procedure Record_Before (S : in out Sink'Class);
   procedure Record_Between (S : in out Sink'Class);
   procedure Record_After (S : in out Sink'Class);

   procedure Put_Image_Unknown (S : in out Sink'Class; Type_Name : String);
   --  For Put_Image of types that don't have the attribute, such as type
   --  Sink.

end System.Put_Images;
