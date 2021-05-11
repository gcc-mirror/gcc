------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                             SYSTEM.PUT_IMAGES                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2020-2021, Free Software Foundation, Inc.       --
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

with Unchecked_Conversion;
with Ada.Strings.Text_Output.Utils;
use Ada.Strings.Text_Output;
use Ada.Strings.Text_Output.Utils;

package body System.Put_Images is

   generic
      type Integer_Type is range <>;
      type Unsigned_Type is mod <>;
      Base : Unsigned_Type;
   package Generic_Integer_Images is
      pragma Assert (Integer_Type'Size = Unsigned_Type'Size);
      pragma Assert (Base in 2 .. 36);
      procedure Put_Image (S : in out Sink'Class; X : Integer_Type);
      procedure Put_Image (S : in out Sink'Class; X : Unsigned_Type);
   private
      subtype Digit is Unsigned_Type range 0 .. Base - 1;
   end Generic_Integer_Images;

   package body Generic_Integer_Images is

      A : constant := Character'Pos ('a');
      Z : constant := Character'Pos ('0');
      function Digit_To_Character (X : Digit) return Character is
        (Character'Val (if X < 10 then X + Z else X + A - 10));

      procedure Put_Digits (S : in out Sink'Class; X : Unsigned_Type);
      --  Put just the digits of X, without any leading minus sign or space.

      procedure Put_Digits (S : in out Sink'Class; X : Unsigned_Type) is
      begin
         if X >= Base then
            Put_Digits (S, X / Base); -- recurse
            Put_7bit (S, Digit_To_Character (X mod Base));
         else
            Put_7bit (S, Digit_To_Character (X));
         end if;
      end Put_Digits;

      procedure Put_Image (S : in out Sink'Class; X : Integer_Type) is
      begin
         --  Put the space or the minus sign, then pass the absolute value to
         --  Put_Digits.

         if X >= 0 then
            Put_7bit (S, ' ');
            Put_Digits (S, Unsigned_Type (X));
         else
            Put_7bit (S, '-');
            Put_Digits (S, -Unsigned_Type'Mod (X));
            --  Convert to Unsigned_Type before negating, to avoid overflow
            --  on Integer_Type'First.
         end if;
      end Put_Image;

      procedure Put_Image (S : in out Sink'Class; X : Unsigned_Type) is
      begin
         Put_7bit (S, ' ');
         Put_Digits (S, X);
      end Put_Image;

   end Generic_Integer_Images;

   package Integer_Images is new Generic_Integer_Images
     (Integer, Unsigned, Base => 10);
   package LL_Integer_Images is new Generic_Integer_Images
     (Long_Long_Integer, Long_Long_Unsigned, Base => 10);
   package LLL_Integer_Images is new Generic_Integer_Images
     (Long_Long_Long_Integer, Long_Long_Long_Unsigned, Base => 10);

   procedure Put_Image_Integer (S : in out Sink'Class; X : Integer)
     renames Integer_Images.Put_Image;
   procedure Put_Image_Long_Long_Integer
     (S : in out Sink'Class; X : Long_Long_Integer)
     renames LL_Integer_Images.Put_Image;
   procedure Put_Image_Long_Long_Long_Integer
     (S : in out Sink'Class; X : Long_Long_Long_Integer)
     renames LLL_Integer_Images.Put_Image;

   procedure Put_Image_Unsigned (S : in out Sink'Class; X : Unsigned)
     renames Integer_Images.Put_Image;
   procedure Put_Image_Long_Long_Unsigned
     (S : in out Sink'Class; X : Long_Long_Unsigned)
     renames LL_Integer_Images.Put_Image;
   procedure Put_Image_Long_Long_Long_Unsigned
     (S : in out Sink'Class; X : Long_Long_Long_Unsigned)
     renames LLL_Integer_Images.Put_Image;

   type Signed_Address is range
     -2**(Standard'Address_Size - 1) .. 2**(Standard'Address_Size - 1) - 1;
   type Unsigned_Address is mod 2**Standard'Address_Size;
   package Hex is new Generic_Integer_Images
     (Signed_Address, Unsigned_Address, Base => 16);

   generic
      type Designated (<>) is private;
      type Pointer is access all Designated;
   procedure Put_Image_Pointer
     (S : in out Sink'Class; X : Pointer; Type_Kind : String);

   procedure Put_Image_Pointer
     (S : in out Sink'Class; X : Pointer; Type_Kind : String)
   is
      function Cast is new Unchecked_Conversion
        (System.Address, Unsigned_Address);
   begin
      if X = null then
         Put_UTF_8 (S, "null");
      else
         Put_UTF_8 (S, "(");
         Put_UTF_8 (S, Type_Kind);
         Hex.Put_Image (S, Cast (X.all'Address));
         Put_UTF_8 (S, ")");
      end if;
   end Put_Image_Pointer;

   procedure Thin_Instance is new Put_Image_Pointer (Byte, Thin_Pointer);
   procedure Put_Image_Thin_Pointer
     (S : in out Sink'Class; X : Thin_Pointer)
   is
   begin
      Thin_Instance (S, X, "access");
   end Put_Image_Thin_Pointer;

   procedure Fat_Instance is new Put_Image_Pointer (Byte_String, Fat_Pointer);
   procedure Put_Image_Fat_Pointer
     (S : in out Sink'Class; X : Fat_Pointer)
   is
   begin
      Fat_Instance (S, X, "access");
   end Put_Image_Fat_Pointer;

   procedure Put_Image_Access_Subp (S : in out Sink'Class; X : Thin_Pointer) is
   begin
      Thin_Instance (S, X, "access subprogram");
   end Put_Image_Access_Subp;

   procedure Put_Image_Access_Prot_Subp
     (S : in out Sink'Class; X : Thin_Pointer)
   is
   begin
      Thin_Instance (S, X, "access protected subprogram");
   end Put_Image_Access_Prot_Subp;

   procedure Put_Image_String (S : in out Sink'Class; X : String) is
   begin
      Put_UTF_8 (S, """");
      for C of X loop
         if C = '"' then
            Put_UTF_8 (S, """");
         end if;
         Put_Character (S, C);
      end loop;
      Put_UTF_8 (S, """");
   end Put_Image_String;

   procedure Put_Image_Wide_String (S : in out Sink'Class; X : Wide_String) is
   begin
      Put_UTF_8 (S, """");
      for C of X loop
         if C = '"' then
            Put_UTF_8 (S, """");
         end if;
         Put_Wide_Character (S, C);
      end loop;
      Put_UTF_8 (S, """");
   end Put_Image_Wide_String;

   procedure Put_Image_Wide_Wide_String
     (S : in out Sink'Class; X : Wide_Wide_String) is
   begin
      Put_UTF_8 (S, """");
      for C of X loop
         if C = '"' then
            Put_UTF_8 (S, """");
         end if;
         Put_Wide_Wide_Character (S, C);
      end loop;
      Put_UTF_8 (S, """");
   end Put_Image_Wide_Wide_String;

   procedure Array_Before (S : in out Sink'Class) is
   begin
      New_Line (S);
      Put_7bit (S, '[');
      Indent (S, 1);
   end Array_Before;

   procedure Array_Between (S : in out Sink'Class) is
   begin
      Put_7bit (S, ',');
      New_Line (S);
   end Array_Between;

   procedure Array_After (S : in out Sink'Class) is
   begin
      Outdent (S, 1);
      Put_7bit (S, ']');
   end Array_After;

   procedure Simple_Array_Between (S : in out Sink'Class) is
   begin
      Put_7bit (S, ',');
      if Column (S) > 60 then
         New_Line (S);
      else
         Put_7bit (S, ' ');
      end if;
   end Simple_Array_Between;

   procedure Record_Before (S : in out Sink'Class) is
   begin
      New_Line (S);
      Put_7bit (S, '(');
      Indent (S, 1);
   end Record_Before;

   procedure Record_Between (S : in out Sink'Class) is
   begin
      Put_7bit (S, ',');
      New_Line (S);
   end Record_Between;

   procedure Record_After (S : in out Sink'Class) is
   begin
      Outdent (S, 1);
      Put_7bit (S, ')');
   end Record_After;

   procedure Put_Arrow (S : in out Sink'Class) is
   begin
      Put_UTF_8 (S, " => ");
   end Put_Arrow;

   procedure Put_Image_Unknown (S : in out Sink'Class; Type_Name : String) is
   begin
      Put_UTF_8 (S, "{");
      Put_String (S, Type_Name);
      Put_UTF_8 (S, " object}");
   end Put_Image_Unknown;

end System.Put_Images;
