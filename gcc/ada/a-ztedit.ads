------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--        A D A . W I D E _ W I D E _ T E X T _ I O . E D I T I N G         --
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

package Ada.Wide_Wide_Text_IO.Editing is

   type Picture is private;

   function Valid
     (Pic_String      : String;
      Blank_When_Zero : Boolean := False) return Boolean;

   function To_Picture
     (Pic_String      : String;
      Blank_When_Zero : Boolean := False) return Picture;

   function Pic_String      (Pic : in Picture) return String;
   function Blank_When_Zero (Pic : in Picture) return Boolean;

   Max_Picture_Length : constant := 64;

   Picture_Error : exception;

   Default_Currency   : constant Wide_Wide_String    := "$";
   Default_Fill       : constant Wide_Wide_Character := ' ';
   Default_Separator  : constant Wide_Wide_Character := ',';
   Default_Radix_Mark : constant Wide_Wide_Character := '.';

   generic
      type Num is delta <> digits <>;
      Default_Currency   : Wide_Wide_String :=
                                Wide_Wide_Text_IO.Editing.Default_Currency;
      Default_Fill       : Wide_Wide_Character :=
                                Wide_Wide_Text_IO.Editing.Default_Fill;
      Default_Separator  : Wide_Wide_Character :=
                                Wide_Wide_Text_IO.Editing.Default_Separator;
      Default_Radix_Mark : Wide_Wide_Character :=
                                Wide_Wide_Text_IO.Editing.Default_Radix_Mark;

   package Decimal_Output is

      function Length
        (Pic      : Picture;
         Currency : Wide_Wide_String := Default_Currency) return Natural;

      function Valid
        (Item     : Num;
         Pic      : Picture;
         Currency : Wide_Wide_String := Default_Currency) return Boolean;

      function Image
        (Item       : Num;
         Pic        : Picture;
         Currency   : Wide_Wide_String    := Default_Currency;
         Fill       : Wide_Wide_Character := Default_Fill;
         Separator  : Wide_Wide_Character := Default_Separator;
         Radix_Mark : Wide_Wide_Character := Default_Radix_Mark)
         return Wide_Wide_String;

      procedure Put
        (File       : File_Type;
         Item       : Num;
         Pic        : Picture;
         Currency   : Wide_Wide_String    := Default_Currency;
         Fill       : Wide_Wide_Character := Default_Fill;
         Separator  : Wide_Wide_Character := Default_Separator;
         Radix_Mark : Wide_Wide_Character := Default_Radix_Mark);

      procedure Put
        (Item       : Num;
         Pic        : Picture;
         Currency   : Wide_Wide_String    := Default_Currency;
         Fill       : Wide_Wide_Character := Default_Fill;
         Separator  : Wide_Wide_Character := Default_Separator;
         Radix_Mark : Wide_Wide_Character := Default_Radix_Mark);

      procedure Put
        (To         : out Wide_Wide_String;
         Item       : Num;
         Pic        : Picture;
         Currency   : Wide_Wide_String    := Default_Currency;
         Fill       : Wide_Wide_Character := Default_Fill;
         Separator  : Wide_Wide_Character := Default_Separator;
         Radix_Mark : Wide_Wide_Character := Default_Radix_Mark);

   end Decimal_Output;

private
   MAX_PICSIZE      : constant := 50;
   MAX_MONEYSIZE    : constant := 10;
   Invalid_Position : constant := -1;

   subtype Pic_Index is Natural range 0 .. MAX_PICSIZE;

   type Picture_Record (Length : Pic_Index := 0) is record
      Expanded : String (1 .. Length);
   end record;

   type Format_Record is record
      Picture              : Picture_Record;
      --  Read only

      Blank_When_Zero      : Boolean;
      --  Read/write

      Original_BWZ         : Boolean;

      --  The following components get written

      Star_Fill            : Boolean := False;

      Radix_Position       : Integer := Invalid_Position;

      Sign_Position,
      Second_Sign          : Integer := Invalid_Position;

      Start_Float,
      End_Float            : Integer := Invalid_Position;

      Start_Currency,
      End_Currency         : Integer := Invalid_Position;

      Max_Leading_Digits   : Integer := 0;

      Max_Trailing_Digits  : Integer := 0;

      Max_Currency_Digits  : Integer := 0;

      Floater              : Wide_Wide_Character := '!';
      --  Initialized to illegal value

   end record;

   type Picture is record
      Contents : Format_Record;
   end record;

   type Number_Attributes is record
      Negative     : Boolean := False;

      Has_Fraction : Boolean := False;

      Start_Of_Int,
      End_Of_Int,
      Start_Of_Fraction,
      End_Of_Fraction : Integer := Invalid_Position;    -- invalid value
   end record;

   function Parse_Number_String (Str : String) return Number_Attributes;
   --  Assumed format is 'IMAGE or Fixed_IO.Put format (depends on no
   --  trailing blanks...)

   procedure Precalculate (Pic : in out Format_Record);
   --  Precalculates fields from the user supplied data

   function Format_Number
     (Pic                 : Format_Record;
      Number              : String;
      Currency_Symbol     : Wide_Wide_String;
      Fill_Character      : Wide_Wide_Character;
      Separator_Character : Wide_Wide_Character;
      Radix_Point         : Wide_Wide_Character) return Wide_Wide_String;
   --  Formats number according to Pic

   function Expand (Picture : in String) return String;

end Ada.Wide_Wide_Text_IO.Editing;
