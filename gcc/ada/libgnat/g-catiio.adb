------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                G N A T . C A L E N D A R . T I M E _ I O                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1999-2018, AdaCore                     --
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

   procedure Parse_ISO_8861_UTC
      (Date    : String;
       Time    : out Ada.Calendar.Time;
       Success : out Boolean);
   --  Subsidiary of function Value. It parses the string Date, interpreted as
   --  an ISO 8861 time representation, and returns corresponding Time value.
   --  Success is set to False when the string is not a supported ISO 8861
   --  date. The following regular expression defines the supported format:
   --
   --    (yyyymmdd | yyyy'-'mm'-'dd)'T'(hhmmss | hh':'mm':'ss)
   --      [ ('Z' | ('.' | ',') s{s} | ('+'|'-')hh':'mm) ]
   --
   --  Trailing characters (in particular spaces) are not allowed.
   --
   --  Examples:
   --
   --    2017-04-14T14:47:06    20170414T14:47:06    20170414T144706
   --    2017-04-14T14:47:06,12 20170414T14:47:06.12
   --    2017-04-14T19:47:06+05 20170414T09:00:06-05:47

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
                     DOW : constant Natural range 0 .. 6 :=
                             (if Day_Of_Week (Date) = Sunday
                              then 0
                              else Day_Name'Pos (Day_Of_Week (Date)));
                  begin
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
      --  Short version of the month names, used when parsing date strings

      S : String := Str;

   begin
      GNAT.Case_Util.To_Upper (S);

      for J in Abbrev_Upper_Month_Names'Range loop
         if Abbrev_Upper_Month_Names (J) = S then
            return J;
         end if;
      end loop;

      return Abbrev_Upper_Month_Names'First;
   end Month_Name_To_Number;

   ------------------------
   -- Parse_ISO_8861_UTC --
   ------------------------

   procedure Parse_ISO_8861_UTC
      (Date    : String;
       Time    : out Ada.Calendar.Time;
       Success : out Boolean)
   is
      Index : Positive := Date'First;
      --  The current character scan index. After a call to Advance, Index
      --  points to the next character.

      End_Of_Source_Reached : exception;
      --  An exception used to signal that the scan pointer has reached the
      --  end of the source string.

      Wrong_Syntax : exception;
      --  An exception used to signal that the scan pointer has reached an
      --  unexpected character in the source string.

      procedure Advance;
      pragma Inline (Advance);
      --  Past the current character of Date

      procedure Advance_Digits (Num_Digits : Positive);
      pragma Inline (Advance_Digits);
      --  Past the given number of digit characters

      function Scan_Day return Day_Number;
      pragma Inline (Scan_Day);
      --  Scan the two digits of a day number and return its value

      function Scan_Hour return Hour_Number;
      pragma Inline (Scan_Hour);
      --  Scan the two digits of an hour number and return its value

      function Scan_Minute return Minute_Number;
      pragma Inline (Scan_Minute);
      --  Scan the two digits of a minute number and return its value

      function Scan_Month return Month_Number;
      pragma Inline (Scan_Month);
      --  Scan the two digits of a month number and return its value

      function Scan_Second return Second_Number;
      pragma Inline (Scan_Second);
      --  Scan the two digits of a second number and return its value

      function Scan_Separator (Expected_Symbol : Character) return Boolean;
      pragma Inline (Scan_Separator);
      --  If the current symbol matches the Expected_Symbol then advance the
      --  scanner index and return True; otherwise do nothing and return False

      procedure Scan_Separator (Required : Boolean; Separator : Character);
      pragma Inline (Scan_Separator);
      --  If Required then check that the current character matches Separator
      --  and advance the scanner index; if not Required then do nothing.

      function Scan_Subsecond return Second_Duration;
      pragma Inline (Scan_Subsecond);
      --  Scan all the digits of a subsecond number and return its value

      function Scan_Year return Year_Number;
      pragma Inline (Scan_Year);
      --  Scan the four digits of a year number and return its value

      function Symbol return Character;
      pragma Inline (Symbol);
      --  Return the current character being scanned

      -------------
      -- Advance --
      -------------

      procedure Advance is
      begin
         --  Signal the end of the source string. This stops a complex scan by
         --  bottoming up any recursive calls till control reaches routine Scan
         --  which handles the exception. Certain scanning scenarios may handle
         --  this exception on their own.

         if Index > Date'Last then
            raise End_Of_Source_Reached;

         --  Advance the scan pointer as long as there are characters to scan,
         --  in other words, the scan pointer has not passed the end of the
         --  source string.

         else
            Index := Index + 1;
         end if;
      end Advance;

      --------------------
      -- Advance_Digits --
      --------------------

      procedure Advance_Digits (Num_Digits : Positive) is
      begin
         for J in 1 .. Num_Digits loop
            if Symbol not in '0' .. '9' then
               raise Wrong_Syntax;
            end if;

            Advance; --  past digit
         end loop;
      end Advance_Digits;

      --------------
      -- Scan_Day --
      --------------

      function Scan_Day return Day_Number is
         From : constant Positive := Index;
      begin
         Advance_Digits (Num_Digits => 2);
         return Day_Number'Value (Date (From .. Index - 1));
      end Scan_Day;

      ---------------
      -- Scan_Hour --
      ---------------

      function Scan_Hour return Hour_Number is
         From : constant Positive := Index;
      begin
         Advance_Digits (Num_Digits => 2);
         return Hour_Number'Value (Date (From .. Index - 1));
      end Scan_Hour;

      -----------------
      -- Scan_Minute --
      -----------------

      function Scan_Minute return Minute_Number is
         From : constant Positive := Index;
      begin
         Advance_Digits (Num_Digits => 2);
         return Minute_Number'Value (Date (From .. Index - 1));
      end Scan_Minute;

      ----------------
      -- Scan_Month --
      ----------------

      function Scan_Month return Month_Number is
         From : constant Positive := Index;
      begin
         Advance_Digits (Num_Digits => 2);
         return Month_Number'Value (Date (From .. Index - 1));
      end Scan_Month;

      -----------------
      -- Scan_Second --
      -----------------

      function Scan_Second return Second_Number is
         From : constant Positive := Index;
      begin
         Advance_Digits (Num_Digits => 2);
         return Second_Number'Value (Date (From .. Index - 1));
      end Scan_Second;

      --------------------
      -- Scan_Separator --
      --------------------

      function Scan_Separator (Expected_Symbol : Character) return Boolean is
      begin
         if Symbol = Expected_Symbol then
            Advance;
            return True;
         else
            return False;
         end if;
      end Scan_Separator;

      --------------------
      -- Scan_Separator --
      --------------------

      procedure Scan_Separator (Required : Boolean; Separator : Character) is
      begin
         if Required then
            if Symbol /= Separator then
               raise Wrong_Syntax;
            end if;

            Advance; --  Past the separator
         end if;
      end Scan_Separator;

      --------------------
      -- Scan_Subsecond --
      --------------------

      function Scan_Subsecond return Second_Duration is
         From : constant Positive := Index;
      begin
         Advance_Digits (Num_Digits => 1);

         while Symbol in '0' .. '9'
           and then Index < Date'Length
         loop
            Advance;
         end loop;

         if Symbol not in '0' .. '9' then
            raise Wrong_Syntax;
         end if;

         Advance;
         return Second_Duration'Value ("0." & Date (From .. Index - 1));
      end Scan_Subsecond;

      ---------------
      -- Scan_Year --
      ---------------

      function Scan_Year return Year_Number is
         From : constant Positive := Index;
      begin
         Advance_Digits (Num_Digits => 4);
         return Year_Number'Value (Date (From .. Index - 1));
      end Scan_Year;

      ------------
      -- Symbol --
      ------------

      function Symbol return Character is
      begin
         --  Signal the end of the source string. This stops a complex scan by
         --  bottoming up any recursive calls till control reaches routine Scan
         --  which handles the exception. Certain scanning scenarios may handle
         --  this exception on their own.

         if Index > Date'Last then
            raise End_Of_Source_Reached;

         else
            return Date (Index);
         end if;
      end Symbol;

      --  Local variables

      Date_Separator : constant Character := '-';
      Hour_Separator : constant Character := ':';

      Day          : Day_Number;
      Month        : Month_Number;
      Year         : Year_Number;
      Hour         : Hour_Number     := 0;
      Minute       : Minute_Number   := 0;
      Second       : Second_Number   := 0;
      Subsec       : Second_Duration := 0.0;

      Local_Hour   : Hour_Number     := 0;
      Local_Minute : Minute_Number   := 0;
      Local_Sign   : Character       := ' ';
      Local_Disp   : Duration;

      Sep_Required : Boolean := False;
      --  True if a separator is seen (and therefore required after it!)

   begin
      --  Parse date

      Year := Scan_Year;
      Sep_Required := Scan_Separator (Date_Separator);

      Month := Scan_Month;
      Scan_Separator (Sep_Required, Date_Separator);

      Day := Scan_Day;

      if Index < Date'Last and then Symbol = 'T' then
         Advance;

         --  Parse time

         Hour := Scan_Hour;
         Sep_Required := Scan_Separator (Hour_Separator);

         Minute := Scan_Minute;
         Scan_Separator (Sep_Required, Hour_Separator);

         Second := Scan_Second;

         --  [('Z' | ('.' | ',') s{s} | ('+'|'-')hh:mm)]

         if Index <= Date'Last then

            --  Suffix 'Z' just confirms that this is an UTC time. No further
            --  action needed.

            if Symbol = 'Z' then
               Advance;

            --  A decimal fraction shall have at least one digit, and has as
            --  many digits as supported by the underlying implementation.
            --  The valid decimal separators are those specified in ISO 31-0,
            --  i.e. the comma [,] or full stop [.]. Of these, the comma is
            --  the preferred separator of ISO-8861.

            elsif Symbol = ',' or else Symbol = '.' then
               Advance; --  past decimal separator
               Subsec := Scan_Subsecond;

            --  Difference between local time and UTC: It shall be expressed
            --  as positive (i.e. with the leading plus sign [+]) if the local
            --  time is ahead of or equal to UTC of day and as negative (i.e.
            --  with the leading minus sign [-]) if it is behind UTC of day.
            --  The minutes time element of the difference may only be omitted
            --  if the difference between the time scales is exactly an
            --  integral number of hours.

            elsif Symbol = '+' or else Symbol = '-' then
               Local_Sign := Symbol;
               Advance;
               Local_Hour := Scan_Hour;

               --  Past ':'

               if Index < Date'Last and then Symbol = Hour_Separator then
                  Advance;
                  Local_Minute := Scan_Minute;
               end if;

               --  Compute local displacement

               Local_Disp := Local_Hour * 3600.0 + Local_Minute * 60.0;
            else
               raise Wrong_Syntax;
            end if;
         end if;
      end if;

      --  Sanity checks. The check on Index ensures that there are no trailing
      --  characters.

      if Index /= Date'Length + 1
        or else not Year'Valid
        or else not Month'Valid
        or else not Day'Valid
        or else not Hour'Valid
        or else not Minute'Valid
        or else not Second'Valid
        or else not Subsec'Valid
        or else not Local_Hour'Valid
        or else not Local_Minute'Valid
      then
         raise Wrong_Syntax;
      end if;

      --  Compute time without local displacement

      if Local_Sign = ' ' then
         Time := Time_Of (Year, Month, Day, Hour, Minute, Second, Subsec);

      --  Compute time with positive local displacement

      elsif Local_Sign = '+' then
         Time :=
           Time_Of (Year, Month, Day, Hour, Minute, Second, Subsec) -
             Local_Disp;

      --  Compute time with negative local displacement

      elsif Local_Sign = '-' then
         Time :=
           Time_Of (Year, Month, Day, Hour, Minute, Second, Subsec) +
             Local_Disp;
      end if;

      --  Notify that the input string was successfully parsed

      Success := True;

   exception
      when End_Of_Source_Reached
         | Wrong_Syntax
      =>
         Success := False;
   end Parse_ISO_8861_UTC;

   -----------
   -- Value --
   -----------

   function Value (Date : String) return Ada.Calendar.Time is
      D          : String (1 .. 21);
      D_Length   : constant Natural := Date'Length;

      Year   : Year_Number;
      Month  : Month_Number;
      Day    : Day_Number;
      Hour   : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;

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

      Success    : Boolean;
      Time_Start : Natural := 1;
      Time       : Ada.Calendar.Time;

   --  Start of processing for Value

   begin
      --  Let's try parsing Date as a supported ISO-8861 format. If we do not
      --  succeed, then retry using all the other GNAT supported formats.

      Parse_ISO_8861_UTC (Date, Time, Success);

      if Success then
         return Time;
      end if;

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

      if D_Length /= 8 or else D (3) /= ':' then
         Extract_Date (Year, Month, Day, Time_Start);
         Extract_Time (Time_Start, Hour, Minute, Second, Check_Space => True);

      else
         declare
            Discard : Second_Duration;
         begin
            Split (Clock, Year, Month, Day, Hour, Minute, Second,
                   Sub_Second => Discard);
         end;

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

      return Time_Of (Year, Month, Day, Hour, Minute, Second);
   end Value;

   --------------
   -- Put_Time --
   --------------

   procedure Put_Time (Date : Ada.Calendar.Time; Picture : Picture_String) is
   begin
      Ada.Text_IO.Put (Image (Date, Picture));
   end Put_Time;

end GNAT.Calendar.Time_IO;
