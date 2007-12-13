------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                G N A T . C A L E N D A R . T I M E _ I O                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1999-2007, AdaCore                     --
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

with Ada.Calendar;            use Ada.Calendar;
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Case_Util;

package body GNAT.Calendar.Time_IO is

   type Month_Name is
     (January,
      February,
      March,
      April,
      May,
      June,
      July,
      August,
      September,
      October,
      November,
      December);

   function Month_Name_To_Number
     (Str : String) return Ada.Calendar.Month_Number;
   --  Converts a string that contains an abbreviated month name to a month
   --  number. Constraint_Error is raised if Str is not a valid month name.
   --  Comparison is case insensitive

   type Padding_Mode is (None, Zero, Space);

   type Sec_Number is mod 2 ** 64;
   --  Type used to compute the number of seconds since 01/01/1970. A 32 bit
   --  number will cover only a period of 136 years. This means that for date
   --  past 2106 the computation is not possible. A 64 bits number should be
   --  enough for a very large period of time.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Am_Pm (H : Natural) return String;
   --  Return AM or PM depending on the hour H

   function Hour_12 (H : Natural) return Positive;
   --  Convert a 1-24h format to a 0-12 hour format

   function Image (Str : String; Length : Natural := 0) return String;
   --  Return Str capitalized and cut to length number of characters. If
   --  length is 0, then no cut operation is performed.

   function Image
     (N       : Sec_Number;
      Padding : Padding_Mode := Zero;
      Length  : Natural := 0) return String;
   --  Return image of N. This number is eventually padded with zeros or spaces
   --  depending of the length required. If length is 0 then no padding occurs.

   function Image
     (N       : Natural;
      Padding : Padding_Mode := Zero;
      Length  : Natural := 0) return String;
   --  As above with N provided in Integer format

   -----------
   -- Am_Pm --
   -----------

   function Am_Pm (H : Natural) return String is
   begin
      if H = 0 or else H > 12 then
         return "PM";
      else
         return "AM";
      end if;
   end Am_Pm;

   -------------
   -- Hour_12 --
   -------------

   function Hour_12 (H : Natural) return Positive is
   begin
      if H = 0 then
         return 12;
      elsif H <= 12 then
         return H;
      else --  H > 12
         return H - 12;
      end if;
   end Hour_12;

   -----------
   -- Image --
   -----------

   function Image
     (Str    : String;
      Length : Natural := 0) return String
   is
      use Ada.Characters.Handling;
      Local : constant String :=
                To_Upper (Str (Str'First)) &
                  To_Lower (Str (Str'First + 1 .. Str'Last));
   begin
      if Length = 0 then
         return Local;
      else
         return Local (1 .. Length);
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (N       : Natural;
      Padding : Padding_Mode := Zero;
      Length  : Natural := 0) return String
   is
   begin
      return Image (Sec_Number (N), Padding, Length);
   end Image;

   function Image
     (N       : Sec_Number;
      Padding : Padding_Mode := Zero;
      Length  : Natural := 0) return String
   is
      function Pad_Char return String;

      --------------
      -- Pad_Char --
      --------------

      function Pad_Char return String is
      begin
         case Padding is
            when None  => return "";
            when Zero  => return "00";
            when Space => return "  ";
         end case;
      end Pad_Char;

      --  Local Declarations

      NI  : constant String := Sec_Number'Image (N);
      NIP : constant String := Pad_Char & NI (2 .. NI'Last);

   --  Start of processing for Image

   begin
      if Length = 0 or else Padding = None then
         return NI (2 .. NI'Last);
      else
         return NIP (NIP'Last - Length + 1 .. NIP'Last);
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (Date    : Ada.Calendar.Time;
      Picture : Picture_String) return String
   is
      Padding : Padding_Mode := Zero;
      --  Padding is set for one directive

      Result : Unbounded_String;

      Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration;

      P : Positive;

   begin
      --  Get current time in split format

      Split (Date, Year, Month, Day, Hour, Minute, Second, Sub_Second);

      --  Null picture string is error

      if Picture = "" then
         raise Picture_Error with "null picture string";
      end if;

      --  Loop through characters of picture string, building result

      Result := Null_Unbounded_String;
      P := Picture'First;
      while P <= Picture'Last loop

         --  A directive has the following format "%[-_]."

         if Picture (P) = '%' then
            Padding := Zero;

            if P = Picture'Last then
               raise Picture_Error with "picture string ends with '%";
            end if;

            --  Check for GNU extension to change the padding

            if Picture (P + 1) = '-' then
               Padding := None;
               P := P + 1;

            elsif Picture (P + 1) = '_' then
               Padding := Space;
               P := P + 1;
            end if;

            if P = Picture'Last then
               raise Picture_Error with "picture string ends with '- or '_";
            end if;

            case Picture (P + 1) is

               --  Literal %

               when '%' =>
                  Result := Result & '%';

               --  A newline

               when 'n' =>
                  Result := Result & ASCII.LF;

               --  A horizontal tab

               when 't' =>
                  Result := Result & ASCII.HT;

               --  Hour (00..23)

               when 'H' =>
                  Result := Result & Image (Hour, Padding, 2);

               --  Hour (01..12)

               when 'I' =>
                  Result := Result & Image (Hour_12 (Hour), Padding, 2);

               --  Hour ( 0..23)

               when 'k' =>
                  Result := Result & Image (Hour, Space, 2);

               --  Hour ( 1..12)

               when 'l' =>
                  Result := Result & Image (Hour_12 (Hour), Space, 2);

               --  Minute (00..59)

               when 'M' =>
                  Result := Result & Image (Minute, Padding, 2);

               --  AM/PM

               when 'p' =>
                  Result := Result & Am_Pm (Hour);

               --  Time, 12-hour (hh:mm:ss [AP]M)

               when 'r' =>
                  Result := Result &
                    Image (Hour_12 (Hour), Padding, Length => 2) & ':' &
                    Image (Minute, Padding, Length => 2) & ':' &
                    Image (Second, Padding, Length => 2) & ' ' &
                    Am_Pm (Hour);

               --   Seconds since 1970-01-01  00:00:00 UTC
               --   (a nonstandard extension)

               when 's' =>
                  declare
                     --  Compute the number of seconds using Ada.Calendar.Time
                     --  values rather than Julian days to account for Daylight
                     --  Savings Time.

                     Neg : Boolean  := False;
                     Sec : Duration := Date - Time_Of (1970, 1, 1, 0.0);

                  begin
                     --  Avoid rounding errors and perform special processing
                     --  for dates earlier than the Unix Epoc.

                     if Sec > 0.0 then
                        Sec := Sec - 0.5;
                     elsif Sec < 0.0 then
                        Neg := True;
                        Sec := abs (Sec + 0.5);
                     end if;

                     --  Prepend a minus sign to the result since Sec_Number
                     --  cannot handle negative numbers.

                     if Neg then
                        Result :=
                          Result & "-" & Image (Sec_Number (Sec), None);
                     else
                        Result := Result & Image (Sec_Number (Sec), None);
                     end if;
                  end;

               --  Second (00..59)

               when 'S' =>
                  Result := Result & Image (Second, Padding, Length => 2);

               --  Milliseconds (3 digits)
               --  Microseconds (6 digits)
               --  Nanoseconds  (9 digits)

               when 'i' | 'e' | 'o' =>
                  declare
                     Sub_Sec : constant Long_Integer :=
                                 Long_Integer (Sub_Second * 1_000_000_000);

                     Img1  : constant String := Sub_Sec'Img;
                     Img2  : constant String :=
                               "00000000" & Img1 (Img1'First + 1 .. Img1'Last);
                     Nanos : constant String :=
                               Img2 (Img2'Last - 8 .. Img2'Last);

                  begin
                     case Picture (P + 1) is
                        when 'i' =>
                           Result := Result &
                             Nanos (Nanos'First .. Nanos'First + 2);

                        when 'e' =>
                           Result := Result &
                             Nanos (Nanos'First .. Nanos'First + 5);

                        when 'o' =>
                           Result := Result & Nanos;

                        when others =>
                           null;
                     end case;
                  end;

               --  Time, 24-hour (hh:mm:ss)

               when 'T' =>
                  Result := Result &
                    Image (Hour, Padding, Length => 2)   & ':' &
                    Image (Minute, Padding, Length => 2) & ':' &
                    Image (Second, Padding, Length => 2);

               --  Locale's abbreviated weekday name (Sun..Sat)

               when 'a' =>
                  Result := Result &
                    Image (Day_Name'Image (Day_Of_Week (Date)), 3);

               --  Locale's full weekday name, variable length
               --  (Sunday..Saturday)

               when 'A' =>
                  Result := Result &
                    Image (Day_Name'Image (Day_Of_Week (Date)));

               --  Locale's abbreviated month name (Jan..Dec)

               when 'b' | 'h' =>
                  Result := Result &
                    Image (Month_Name'Image (Month_Name'Val (Month - 1)), 3);

               --  Locale's full month name, variable length
               --  (January..December).

               when 'B' =>
                  Result := Result &
                    Image (Month_Name'Image (Month_Name'Val (Month - 1)));

               --  Locale's date and time (Sat Nov 04 12:02:33 EST 1989)

               when 'c' =>
                  case Padding is
                     when Zero =>
                        Result := Result & Image (Date, "%a %b %d %T %Y");
                     when Space =>
                        Result := Result & Image (Date, "%a %b %_d %_T %Y");
                     when None =>
                        Result := Result & Image (Date, "%a %b %-d %-T %Y");
                  end case;

               --   Day of month (01..31)

               when 'd' =>
                  Result := Result & Image (Day, Padding, 2);

               --  Date (mm/dd/yy)

               when 'D' | 'x' =>
                  Result := Result &
                              Image (Month, Padding, 2) & '/' &
                              Image (Day, Padding, 2) & '/' &
                              Image (Year, Padding, 2);

               --  Day of year (001..366)

               when 'j' =>
                  Result := Result & Image (Day_In_Year (Date), Padding, 3);

               --  Month (01..12)

               when 'm' =>
                  Result := Result & Image (Month, Padding, 2);

               --  Week number of year with Sunday as first day of week
               --  (00..53)

               when 'U' =>
                  declare
                     Offset : constant Natural :=
                                (Julian_Day (Year, 1, 1) + 1) mod 7;

                     Week : constant Natural :=
                              1 + ((Day_In_Year (Date) - 1) + Offset) / 7;

                  begin
                     Result := Result & Image (Week, Padding, 2);
                  end;

               --  Day of week (0..6) with 0 corresponding to Sunday

               when 'w' =>
                  declare
                     DOW : Natural range 0 .. 6;

                  begin
                     if Day_Of_Week (Date) = Sunday then
                        DOW := 0;
                     else
                        DOW := Day_Name'Pos (Day_Of_Week (Date));
                     end if;

                     Result := Result & Image (DOW, Length => 1);
                  end;

               --  Week number of year with Monday as first day of week
               --  (00..53)

               when 'W' =>
                  Result := Result & Image (Week_In_Year (Date), Padding, 2);

               --  Last two digits of year (00..99)

               when 'y' =>
                  declare
                     Y : constant Natural := Year - (Year / 100) * 100;
                  begin
                     Result := Result & Image (Y, Padding, 2);
                  end;

               --   Year (1970...)

               when 'Y' =>
                  Result := Result & Image (Year, None, 4);

               when others =>
                  raise Picture_Error with
                    "unknown format character in picture string";

            end case;

            --  Skip past % and format character

            P := P + 2;

         --  Character other than % is copied into the result

         else
            Result := Result & Picture (P);
            P := P + 1;
         end if;
      end loop;

      return To_String (Result);
   end Image;

   --------------------------
   -- Month_Name_To_Number --
   --------------------------

   function Month_Name_To_Number
     (Str : String) return Ada.Calendar.Month_Number
   is
      subtype String3 is String (1 .. 3);
      Abbrev_Upper_Month_Names :
        constant array (Ada.Calendar.Month_Number) of String3 :=
         ("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
          "JUL", "AUG", "SEP", "OCT", "NOV", "DEC");
      --  Short version of the month names, used when parsing date strings.

      S                                                     : String := Str;

   begin
      GNAT.Case_Util.To_Upper (S);

      for J in Abbrev_Upper_Month_Names'Range loop
         if Abbrev_Upper_Month_Names (J) = S then
            return J;
         end if;
      end loop;

      return Abbrev_Upper_Month_Names'First;
   end Month_Name_To_Number;

   -----------
   -- Value --
   -----------

   function Value (Date : String) return Ada.Calendar.Time is
      D          : String (1 .. 21);
      D_Length   : constant Natural := Date'Length;

      Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration;

      procedure Extract_Date
        (Year       : out Year_Number;
         Month      : out Month_Number;
         Day        : out Day_Number;
         Time_Start : out Natural);
      --  Try and extract a date value from string D. Time_Start is set to the
      --  first character that could be the start of time data.

      procedure Extract_Time
        (Index       : Positive;
         Hour        : out Hour_Number;
         Minute      : out Minute_Number;
         Second      : out Second_Number;
         Check_Space : Boolean := False);
      --  Try and extract a time value from string D starting from position
      --  Index. Set Check_Space to True to check whether the character at
      --  Index - 1 is a space. Raise Constraint_Error if the portion of D
      --  corresponding to the date is not well formatted.

      ------------------
      -- Extract_Date --
      ------------------

      procedure Extract_Date
        (Year       : out Year_Number;
         Month      : out Month_Number;
         Day        : out Day_Number;
         Time_Start : out Natural)
      is
      begin
         if D (3) = '-' or else D (3) = '/' then
            if D_Length = 8 or else D_Length = 17 then

               --  Formats are "yy*mm*dd" or "yy*mm*dd hh:mm:ss"

               if D (6) /= D (3) then
                  raise Constraint_Error;
               end if;

               Year  := Year_Number'Value ("20" & D (1 .. 2));
               Month := Month_Number'Value       (D (4 .. 5));
               Day   := Day_Number'Value         (D (7 .. 8));
               Time_Start := 10;

            elsif D_Length = 10 or else D_Length = 19 then

               --  Formats are "mm*dd*yyyy" or "mm*dd*yyyy hh:mm:ss"

               if D (6) /= D (3) then
                  raise Constraint_Error;
               end if;

               Year  := Year_Number'Value  (D (7 .. 10));
               Month := Month_Number'Value (D (1 .. 2));
               Day   := Day_Number'Value   (D (4 .. 5));
               Time_Start := 12;

            elsif D_Length = 11 or else D_Length = 20 then

               --  Formats are "dd*mmm*yyyy" or "dd*mmm*yyyy hh:mm:ss"

               if D (7) /= D (3) then
                  raise Constraint_Error;
               end if;

               Year  := Year_Number'Value  (D (8 .. 11));
               Month := Month_Name_To_Number (D (4 .. 6));
               Day   := Day_Number'Value   (D (1 .. 2));
               Time_Start := 13;

            else
               raise Constraint_Error;
            end if;

         elsif D (3) = ' ' then
            if D_Length = 11 or else D_Length = 20 then

               --  Possible formats are "dd mmm yyyy", "dd mmm yyyy hh:mm:ss"

               if D (7) /= ' ' then
                  raise Constraint_Error;
               end if;

               Year  := Year_Number'Value  (D (8 .. 11));
               Month := Month_Name_To_Number (D (4 .. 6));
               Day   := Day_Number'Value   (D (1 .. 2));
               Time_Start := 13;

            else
               raise Constraint_Error;
            end if;

         else
            if D_Length = 8 or else D_Length = 17 then

               --  Possible formats are "yyyymmdd" or "yyyymmdd hh:mm:ss"

               Year  := Year_Number'Value (D (1 .. 4));
               Month := Month_Number'Value (D (5 .. 6));
               Day   := Day_Number'Value (D (7 .. 8));
               Time_Start := 10;

            elsif D_Length = 10 or else D_Length = 19 then

               --  Possible formats are "yyyy*mm*dd" or "yyyy*mm*dd hh:mm:ss"

               if (D (5) /= '-' and then D (5) /= '/')
                 or else D (8) /= D (5)
               then
                  raise Constraint_Error;
               end if;

               Year  := Year_Number'Value (D (1 .. 4));
               Month := Month_Number'Value (D (6 .. 7));
               Day   := Day_Number'Value (D (9 .. 10));
               Time_Start := 12;

            elsif D_Length = 11 or else D_Length = 20 then

               --  Possible formats are "yyyy*mmm*dd"

               if (D (5) /= '-' and then D (5) /= '/')
                 or else D (9) /= D (5)
               then
                  raise Constraint_Error;
               end if;

               Year  := Year_Number'Value (D (1 .. 4));
               Month := Month_Name_To_Number (D (6 .. 8));
               Day   := Day_Number'Value (D (10 .. 11));
               Time_Start := 13;

            elsif D_Length = 12 or else D_Length = 21 then

               --  Formats are "mmm dd, yyyy" or "mmm dd, yyyy hh:mm:ss"

               if D (4) /= ' '
                 or else D (7) /= ','
                 or else D (8) /= ' '
               then
                  raise Constraint_Error;
               end if;

               Year  := Year_Number'Value (D (9 .. 12));
               Month := Month_Name_To_Number (D (1 .. 3));
               Day   := Day_Number'Value (D (5 .. 6));
               Time_Start := 14;

            else
               raise Constraint_Error;
            end if;
         end if;
      end Extract_Date;

      ------------------
      -- Extract_Time --
      ------------------

      procedure Extract_Time
        (Index       : Positive;
         Hour        : out Hour_Number;
         Minute      : out Minute_Number;
         Second      : out Second_Number;
         Check_Space : Boolean := False)
      is
      begin
         --  If no time was specified in the string (do not allow trailing
         --  character either)

         if Index = D_Length + 2 then
            Hour   := 0;
            Minute := 0;
            Second := 0;

         else
            --  Not enough characters left ?

            if Index /= D_Length - 7 then
               raise Constraint_Error;
            end if;

            if Check_Space and then D (Index - 1) /= ' ' then
               raise Constraint_Error;
            end if;

            if D (Index + 2) /= ':' or else D (Index + 5) /= ':' then
               raise Constraint_Error;
            end if;

            Hour   := Hour_Number'Value   (D (Index     .. Index + 1));
            Minute := Minute_Number'Value (D (Index + 3 .. Index + 4));
            Second := Second_Number'Value (D (Index + 6 .. Index + 7));
         end if;
      end Extract_Time;

      --  Local Declarations

      Time_Start : Natural := 1;

   --  Start of processing for Value

   begin
      Split (Clock, Year, Month, Day, Hour, Minute, Second, Sub_Second);
      Sub_Second := 0.0;

      --  Length checks

      if D_Length /= 8
        and then D_Length /= 10
        and then D_Length /= 11
        and then D_Length /= 12
        and then D_Length /= 17
        and then D_Length /= 19
        and then D_Length /= 20
        and then D_Length /= 21
      then
         raise Constraint_Error;
      end if;

      --  After the correct length has been determined, it is safe to create
      --  a local string copy in order to avoid String'First N arithmetic.

      D (1 .. D_Length) := Date;

      if D_Length /= 8
        or else D (3) /= ':'
      then
         Extract_Date (Year, Month, Day, Time_Start);
         Extract_Time (Time_Start, Hour, Minute, Second, Check_Space => True);
      else
         Extract_Time (1, Hour, Minute, Second, Check_Space => False);
      end if;

      --  Sanity checks

      if not Year'Valid
        or else not Month'Valid
        or else not Day'Valid
        or else not Hour'Valid
        or else not Minute'Valid
        or else not Second'Valid
      then
         raise Constraint_Error;
      end if;

      return Time_Of (Year, Month, Day, Hour, Minute, Second, Sub_Second);
   end Value;

   --------------
   -- Put_Time --
   --------------

   procedure Put_Time
     (Date    : Ada.Calendar.Time;
      Picture : Picture_String)
   is
   begin
      Ada.Text_IO.Put (Image (Date, Picture));
   end Put_Time;

end GNAT.Calendar.Time_IO;
