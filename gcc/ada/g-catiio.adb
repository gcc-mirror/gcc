------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                G N A T . C A L E N D A R . T I M E _ I O                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1999-2006, AdaCore                     --
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

      P : Positive := Picture'First;

   begin
      Split (Date, Year, Month, Day, Hour, Minute, Second, Sub_Second);

      loop
         --  A directive has the following format "%[-_]."

         if Picture (P) = '%' then
            Padding := Zero;

            if P = Picture'Last then
               raise Picture_Error;
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
               raise Picture_Error;
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
                     Sec : constant Sec_Number :=
                             Sec_Number (Julian_Day (Year, Month, Day)
                                          - Julian_Day (1970, 1, 1)) * 86_400
                                          + Sec_Number (Hour) * 3_600
                                          + Sec_Number (Minute) * 60
                                          + Sec_Number (Second);

                  begin
                     Result := Result & Image (Sec, None);
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
                  raise Picture_Error;
            end case;

            P := P + 2;

         else
            Result := Result & Picture (P);
            P := P + 1;
         end if;

         exit when P > Picture'Last;

      end loop;

      return To_String (Result);
   end Image;

   -----------
   -- Value --
   -----------

   function Value (Date : String) return Ada.Calendar.Time is
      D          : String (1 .. 19);
      D_Length   : constant Natural := Date'Length;

      Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration;

      procedure Extract_Date
        (Year  : out Year_Number;
         Month : out Month_Number;
         Day   : out Day_Number;
         Y2K   : Boolean := False);
      --  Try and extract a date value from string D. Set Y2K to True to
      --  account for the 20YY case. Raise Constraint_Error if the portion
      --  of D corresponding to the date is not well formatted.

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
        (Year  : out Year_Number;
         Month : out Month_Number;
         Day   : out Day_Number;
         Y2K   : Boolean := False)
      is
         Delim_Index : Positive := 5;

      begin
         if Y2K then
            Delim_Index := 3;
         end if;

         if (D (Delim_Index) /= '-' or else D (Delim_Index + 3) /= '-')
           and then
            (D (Delim_Index) /= '/' or else D (Delim_Index + 3) /= '/')
         then
            raise Constraint_Error;
         end if;

         if Y2K then
            Year  := Year_Number'Value ("20" & D (1 .. 2));
            Month := Month_Number'Value       (D (4 .. 5));
            Day   := Day_Number'Value         (D (7 .. 8));
         else
            Year  := Year_Number'Value  (D (1 .. 4));
            Month := Month_Number'Value (D (6 .. 7));
            Day   := Day_Number'Value   (D (9 .. 10));
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
         Check_Space : Boolean := False) is

      begin
         if Check_Space and then D (Index - 1) /= ' ' then
            raise Constraint_Error;
         end if;

         if D (Index + 2) /= ':' or else D (Index + 5) /= ':' then
            raise Constraint_Error;
         end if;

         Hour   := Hour_Number'Value   (D (Index     .. Index + 1));
         Minute := Minute_Number'Value (D (Index + 3 .. Index + 4));
         Second := Second_Number'Value (D (Index + 6 .. Index + 7));
      end Extract_Time;

   --  Start of processing for Value

   begin
      Split (Clock, Year, Month, Day, Hour, Minute, Second, Sub_Second);
      Sub_Second := 0.0;

      --  Length checks

      if D_Length /= 8
        and then D_Length /= 10
        and then D_Length /= 17
        and then D_Length /= 19
      then
         raise Constraint_Error;
      end if;

      --  After the correct length has been determined, it is safe to create
      --  a local string copy in order to avoid String'First N arithmetic.

      D (1 .. D_Length) := Date;

      --  Case 1:

      --    hh:mm:ss
      --    yy*mm*dd

      if D_Length = 8 then

         if D (3) = ':' then
            Extract_Time (1, Hour, Minute, Second);
         else
            Extract_Date (Year, Month, Day, True);
            Hour   := 0;
            Minute := 0;
            Second := 0;
         end if;

      --  Case 2:

      --    yyyy*mm*dd

      elsif D_Length = 10 then
         Extract_Date (Year, Month, Day);
         Hour   := 0;
         Minute := 0;
         Second := 0;

      --  Case 3:

      --    yy*mm*dd hh:mm:ss

      elsif D_Length = 17 then
         Extract_Date (Year, Month, Day, True);
         Extract_Time (10, Hour, Minute, Second, True);

      --  Case 4:

      --    yyyy*mm*dd hh:mm:ss

      else
         Extract_Date (Year, Month, Day);
         Extract_Time (12, Hour, Minute, Second, True);
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
