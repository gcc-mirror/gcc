------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 G N A T . B Y T E _ O R D E R _ M A R K                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2006-2017, AdaCore                     --
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

pragma Compiler_Unit_Warning;

package body GNAT.Byte_Order_Mark is

   --------------
   -- Read_BOM --
   --------------

   procedure Read_BOM
     (Str         : String;
      Len         : out Natural;
      BOM         : out BOM_Kind;
      XML_Support : Boolean := False)
   is
   begin
      --  Note: the order of these tests is important, because in some cases
      --  one sequence is a prefix of a longer sequence, and we must test for
      --  the longer sequence first

      --  UTF-32 (big-endian)

      if Str'Length >= 4
        and then Str (Str'First)     = Character'Val (16#00#)
        and then Str (Str'First + 1) = Character'Val (16#00#)
        and then Str (Str'First + 2) = Character'Val (16#FE#)
        and then Str (Str'First + 3) = Character'Val (16#FF#)
      then
         Len := 4;
         BOM := UTF32_BE;

      --  UTF-32 (little-endian)

      elsif Str'Length >= 4
        and then Str (Str'First)     = Character'Val (16#FF#)
        and then Str (Str'First + 1) = Character'Val (16#FE#)
        and then Str (Str'First + 2) = Character'Val (16#00#)
        and then Str (Str'First + 3) = Character'Val (16#00#)
      then
         Len := 4;
         BOM := UTF32_LE;

      --  UTF-16 (big-endian)

      elsif Str'Length >= 2
        and then Str (Str'First) = Character'Val (16#FE#)
        and then Str (Str'First + 1) = Character'Val (16#FF#)
      then
         Len := 2;
         BOM := UTF16_BE;

      --  UTF-16 (little-endian)

      elsif Str'Length >= 2
        and then Str (Str'First) = Character'Val (16#FF#)
        and then Str (Str'First + 1) = Character'Val (16#FE#)
      then
         Len := 2;
         BOM := UTF16_LE;

      --  UTF-8 (endian-independent)

      elsif Str'Length >= 3
        and then Str (Str'First)     = Character'Val (16#EF#)
        and then Str (Str'First + 1) = Character'Val (16#BB#)
        and then Str (Str'First + 2) = Character'Val (16#BF#)
      then
         Len := 3;
         BOM := UTF8_All;

      --  UCS-4 (big-endian) XML only

      elsif XML_Support
        and then Str'Length >= 4
        and then Str (Str'First)     = Character'Val (16#00#)
        and then Str (Str'First + 1) = Character'Val (16#00#)
        and then Str (Str'First + 2) = Character'Val (16#00#)
        and then Str (Str'First + 3) = Character'Val (16#3C#)
      then
         Len := 0;
         BOM := UCS4_BE;

      --  UCS-4 (little-endian) XML case

      elsif XML_Support
        and then Str'Length >= 4
        and then Str (Str'First)     = Character'Val (16#3C#)
        and then Str (Str'First + 1) = Character'Val (16#00#)
        and then Str (Str'First + 2) = Character'Val (16#00#)
        and then Str (Str'First + 3) = Character'Val (16#00#)
      then
         Len := 0;
         BOM := UCS4_LE;

      --  UCS-4 (unusual byte order 2143) XML case

      elsif XML_Support
        and then Str'Length >= 4
        and then Str (Str'First)     = Character'Val (16#00#)
        and then Str (Str'First + 1) = Character'Val (16#00#)
        and then Str (Str'First + 2) = Character'Val (16#3C#)
        and then Str (Str'First + 3) = Character'Val (16#00#)
      then
         Len := 0;
         BOM := UCS4_2143;

      --  UCS-4 (unusual byte order 3412) XML case

      elsif XML_Support
        and then Str'Length >= 4
        and then Str (Str'First)     = Character'Val (16#00#)
        and then Str (Str'First + 1) = Character'Val (16#3C#)
        and then Str (Str'First + 2) = Character'Val (16#00#)
        and then Str (Str'First + 3) = Character'Val (16#00#)
      then
         Len := 0;
         BOM := UCS4_3412;

      --  UTF-16 (big-endian) XML case

      elsif XML_Support
        and then Str'Length >= 4
        and then Str (Str'First)     = Character'Val (16#00#)
        and then Str (Str'First + 1) = Character'Val (16#3C#)
        and then Str (Str'First + 2) = Character'Val (16#00#)
        and then Str (Str'First + 3) = Character'Val (16#3F#)
      then
         Len := 0;
         BOM := UTF16_BE;

      --  UTF-32 (little-endian) XML case

      elsif XML_Support
        and then Str'Length >= 4
        and then Str (Str'First)     = Character'Val (16#3C#)
        and then Str (Str'First + 1) = Character'Val (16#00#)
        and then Str (Str'First + 2) = Character'Val (16#3F#)
        and then Str (Str'First + 3) = Character'Val (16#00#)
      then
         Len := 0;
         BOM := UTF16_LE;

      --  Unrecognized special encodings XML only

      elsif XML_Support
        and then Str'Length >= 4
        and then Str (Str'First)     = Character'Val (16#3C#)
        and then Str (Str'First + 1) = Character'Val (16#3F#)
        and then Str (Str'First + 2) = Character'Val (16#78#)
        and then Str (Str'First + 3) = Character'Val (16#6D#)
      then
         --  UTF-8, ASCII, some part of ISO8859, Shift-JIS, EUC,...

         Len := 0;
         BOM := Unknown;

      --  No BOM recognized

      else
         Len := 0;
         BOM := Unknown;
      end if;
   end Read_BOM;

end GNAT.Byte_Order_Mark;
