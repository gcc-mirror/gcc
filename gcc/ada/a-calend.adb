------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         A D A . C A L E N D A R                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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

with Unchecked_Conversion;

with System.OS_Primitives;
--  used for Clock

package body Ada.Calendar is

   ------------------------------
   -- Use of Pragma Unsuppress --
   ------------------------------

   --  This implementation of Calendar takes advantage of the permission in
   --  Ada 95 of using arithmetic overflow checks to check for out of bounds
   --  time values. This means that we must catch the constraint error that
   --  results from arithmetic overflow, so we use pragma Unsuppress to make
   --  sure that overflow is enabled, using software overflow checking if
   --  necessary. That way, compiling Calendar with options to suppress this
   --  checking will not affect its correctness.

   ------------------------
   -- Local Declarations --
   ------------------------

   type Char_Pointer is access Character;
   subtype int  is Integer;
   subtype long is Long_Integer;
   --  Synonyms for C types. We don't want to get them from Interfaces.C
   --  because there is no point in loading that unit just for calendar.

   type tm is record
      tm_sec    : int;           -- seconds after the minute (0 .. 60)
      tm_min    : int;           -- minutes after the hour (0 .. 59)
      tm_hour   : int;           -- hours since midnight (0 .. 24)
      tm_mday   : int;           -- day of the month (1 .. 31)
      tm_mon    : int;           -- months since January (0 .. 11)
      tm_year   : int;           -- years since 1900
      tm_wday   : int;           -- days since Sunday (0 .. 6)
      tm_yday   : int;           -- days since January 1 (0 .. 365)
      tm_isdst  : int;           -- Daylight Savings Time flag (-1 .. +1)
      tm_gmtoff : long;          -- offset from CUT in seconds
      tm_zone   : Char_Pointer;  -- timezone abbreviation
   end record;

   type tm_Pointer is access all tm;

   subtype time_t is long;

   type time_t_Pointer is access all time_t;

   procedure localtime_r (C : time_t_Pointer; res : tm_Pointer);
   pragma Import (C, localtime_r, "__gnat_localtime_r");

   function mktime (TM : tm_Pointer) return time_t;
   pragma Import (C, mktime);
   --  mktime returns -1 in case the calendar time given by components of
   --  TM.all cannot be represented.

   --  The following constants are used in adjusting Ada dates so that they
   --  fit into the range that can be handled by Unix (1970 - 2038). The trick
   --  is that the number of days in any four year period in the Ada range of
   --  years (1901 - 2099) has a constant number of days. This is because we
   --  have the special case of 2000 which, contrary to the normal exception
   --  for centuries, is a leap year after all.

   Unix_Year_Min : constant := 1970;
   Unix_Year_Max : constant := 2038;

   Ada_Year_Min : constant := 1901;
   Ada_Year_Max : constant := 2099;

   --  Some basic constants used throughout

   Days_In_Month : constant array (Month_Number) of Day_Number :=
                     (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

   Days_In_4_Years     : constant := 365 * 3 + 366;
   Seconds_In_4_Years  : constant := 86_400 * Days_In_4_Years;
   Seconds_In_4_YearsD : constant Duration := Duration (Seconds_In_4_Years);

   ---------
   -- "+" --
   ---------

   function "+" (Left : Time; Right : Duration) return Time is
      pragma Unsuppress (Overflow_Check);
   begin
      return (Left + Time (Right));

   exception
      when Constraint_Error =>
         raise Time_Error;
   end "+";

   function "+" (Left : Duration; Right : Time) return Time is
      pragma Unsuppress (Overflow_Check);
   begin
      return (Time (Left) + Right);

   exception
      when Constraint_Error =>
         raise Time_Error;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left : Time; Right : Duration)  return Time is
      pragma Unsuppress (Overflow_Check);
   begin
      return Left - Time (Right);

   exception
      when Constraint_Error =>
         raise Time_Error;
   end "-";

   function "-" (Left : Time; Right : Time) return Duration is
      pragma Unsuppress (Overflow_Check);
   begin
      return Duration (Left) - Duration (Right);

   exception
      when Constraint_Error =>
         raise Time_Error;
   end "-";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Time) return Boolean is
   begin
      return Duration (Left) < Duration (Right);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Time) return Boolean is
   begin
      return Duration (Left) <= Duration (Right);
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Time) return Boolean is
   begin
      return Duration (Left) > Duration (Right);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Time) return Boolean is
   begin
      return Duration (Left) >= Duration (Right);
   end ">=";

   -----------
   -- Clock --
   -----------

   function Clock return Time is
   begin
      return Time (System.OS_Primitives.Clock);
   end Clock;

   ---------
   -- Day --
   ---------

   function Day (Date : Time) return Day_Number is
      DY : Year_Number;
      DM : Month_Number;
      DD : Day_Number;
      DS : Day_Duration;

   begin
      Split (Date, DY, DM, DD, DS);
      return DD;
   end Day;

   -----------
   -- Month --
   -----------

   function Month (Date : Time) return Month_Number is
      DY : Year_Number;
      DM : Month_Number;
      DD : Day_Number;
      DS : Day_Duration;

   begin
      Split (Date, DY, DM, DD, DS);
      return DM;
   end Month;

   -------------
   -- Seconds --
   -------------

   function Seconds (Date : Time) return Day_Duration is
      DY : Year_Number;
      DM : Month_Number;
      DD : Day_Number;
      DS : Day_Duration;

   begin
      Split (Date, DY, DM, DD, DS);
      return DS;
   end Seconds;

   -----------
   -- Split --
   -----------

   procedure Split
     (Date    : Time;
      Year    : out Year_Number;
      Month   : out Month_Number;
      Day     : out Day_Number;
      Seconds : out Day_Duration)
   is
      --  The following declare bounds for duration that are comfortably
      --  wider than the maximum allowed output result for the Ada range
      --  of representable split values. These are used for a quick check
      --  that the value is not wildly out of range.

      Low  : constant := (Ada_Year_Min - Unix_Year_Min - 2) * 365 * 86_400;
      High : constant := (Ada_Year_Max - Unix_Year_Min + 2) * 365 * 86_400;

      LowD  : constant Duration := Duration (Low);
      HighD : constant Duration := Duration (High);

      --  The following declare the maximum duration value that can be
      --  successfully converted to a 32-bit integer suitable for passing
      --  to the localtime_r function. Note that we cannot assume that the
      --  localtime_r function expands to accept 64-bit input on a 64-bit
      --  machine, but we can count on a 32-bit range on all machines.

      Max_Time  : constant := 2 ** 31 - 1;
      Max_TimeD : constant Duration := Duration (Max_Time);

      --  Finally the actual variables used in the computation

      D                : Duration;
      Frac_Sec         : Duration;
      Year_Val         : Integer;
      Adjusted_Seconds : aliased time_t;
      Tm_Val           : aliased tm;

   begin
      --  For us a time is simply a signed duration value, so we work with
      --  this duration value directly. Note that it can be negative.

      D := Duration (Date);

      --  First of all, filter out completely ludicrous values. Remember
      --  that we use the full stored range of duration values, which may
      --  be significantly larger than the allowed range of Ada times. Note
      --  that these checks are wider than required to make absolutely sure
      --  that there are no end effects from time zone differences.

      if D < LowD or else D > HighD then
         raise Time_Error;
      end if;

      --  The unix localtime_r function is more or less exactly what we need
      --  here. The less comes from the fact that it does not support the
      --  required range of years (the guaranteed range available is only
      --  EPOCH through EPOCH + N seconds). N is in practice 2 ** 31 - 1.

      --  If we have a value outside this range, then we first adjust it
      --  to be in the required range by adding multiples of four years.
      --  For the range we are interested in, the number of days in any
      --  consecutive four year period is constant. Then we do the split
      --  on the adjusted value, and readjust the years value accordingly.

      Year_Val := 0;

      while D < 0.0 loop
         D := D + Seconds_In_4_YearsD;
         Year_Val := Year_Val - 4;
      end loop;

      while D > Max_TimeD loop
         D := D - Seconds_In_4_YearsD;
         Year_Val := Year_Val + 4;
      end loop;

      --  Now we need to take the value D, which is now non-negative, and
      --  break it down into seconds (to pass to the localtime_r function)
      --  and fractions of seconds (for the adjustment below).

      --  Surprisingly there is no easy way to do this in Ada, and certainly
      --  no easy way to do it and generate efficient code. Therefore we
      --  do it at a low level, knowing that it is really represented as
      --  an integer with units of Small

      declare
         type D_Int is range 0 .. 2 ** (Duration'Size - 1) - 1;
         for D_Int'Size use Duration'Size;

         Small_Div : constant D_Int := D_Int (1.0 / Duration'Small);
         D_As_Int  : D_Int;

         function To_D_As_Int is new Unchecked_Conversion (Duration, D_Int);
         function To_Duration is new Unchecked_Conversion (D_Int, Duration);

      begin
         D_As_Int := To_D_As_Int (D);
         Adjusted_Seconds := time_t (D_As_Int / Small_Div);
         Frac_Sec := To_Duration (D_As_Int rem Small_Div);
      end;

      localtime_r (Adjusted_Seconds'Unchecked_Access, Tm_Val'Unchecked_Access);

      Year_Val := Tm_Val.tm_year + 1900 + Year_Val;
      Month    := Tm_Val.tm_mon + 1;
      Day      := Tm_Val.tm_mday;

      --  The Seconds value is a little complex. The localtime function
      --  returns the integral number of seconds, which is what we want,
      --  but we want to retain the fractional part from the original
      --  Time value, since this is typically stored more accurately.

      Seconds := Duration (Tm_Val.tm_hour * 3600 +
                           Tm_Val.tm_min  * 60 +
                           Tm_Val.tm_sec)
                   + Frac_Sec;

      --  Note: the above expression is pretty horrible, one of these days
      --  we should stop using time_of and do everything ourselves to avoid
      --  these unnecessary divides and multiplies???.

      --  The Year may still be out of range, since our entry test was
      --  deliberately crude. Trying to make this entry test accurate is
      --  tricky due to time zone adjustment issues affecting the exact
      --  boundary. It is interesting to note that whether or not a given
      --  Calendar.Time value gets Time_Error when split depends on the
      --  current time zone setting.

      if Year_Val not in Ada_Year_Min .. Ada_Year_Max then
         raise Time_Error;
      else
         Year := Year_Val;
      end if;
   end Split;

   -------------
   -- Time_Of --
   -------------

   function Time_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration := 0.0)
      return    Time
   is
      Result_Secs : aliased time_t;
      TM_Val      : aliased tm;
      Int_Secs    : constant Integer := Integer (Seconds);

      Year_Val        : Integer := Year;
      Duration_Adjust : Duration := 0.0;

   begin
      --  The following checks are redundant with respect to the constraint
      --  error checks that should normally be made on parameters, but we
      --  decide to raise Constraint_Error in any case if bad values come
      --  in (as a result of checks being off in the caller, or for other
      --  erroneous or bounded error cases).

      if        not Year   'Valid
        or else not Month  'Valid
        or else not Day    'Valid
        or else not Seconds'Valid
      then
         raise Constraint_Error;
      end if;

      --  Check for Day value too large (one might expect mktime to do this
      --  check, as well as the basi checks we did with 'Valid, but it seems
      --  that at least on some systems, this built-in check is too weak).

      if Day > Days_In_Month (Month)
        and then (Day /= 29 or Month /= 2 or Year mod 4 /= 0)
      then
         raise Time_Error;
      end if;

      TM_Val.tm_sec  := Int_Secs mod 60;
      TM_Val.tm_min  := (Int_Secs / 60) mod 60;
      TM_Val.tm_hour := (Int_Secs / 60) / 60;
      TM_Val.tm_mday := Day;
      TM_Val.tm_mon  := Month - 1;

      --  For the year, we have to adjust it to a year that Unix can handle.
      --  We do this in four year steps, since the number of days in four
      --  years is constant, so the timezone effect on the conversion from
      --  local time to GMT is unaffected.

      while Year_Val <= Unix_Year_Min loop
         Year_Val := Year_Val + 4;
         Duration_Adjust := Duration_Adjust - Seconds_In_4_YearsD;
      end loop;

      while Year_Val >= Unix_Year_Max loop
         Year_Val := Year_Val - 4;
         Duration_Adjust := Duration_Adjust + Seconds_In_4_YearsD;
      end loop;

      TM_Val.tm_year := Year_Val - 1900;

      --  Since we do not have information on daylight savings,
      --  rely on the default information.

      TM_Val.tm_isdst := -1;
      Result_Secs := mktime (TM_Val'Unchecked_Access);

      --  That gives us the basic value in seconds. Two adjustments are
      --  needed. First we must undo the year adjustment carried out above.
      --  Second we put back the fraction seconds value since in general the
      --  Day_Duration value we received has additional precision which we
      --  do not want to lose in the constructed result.

      return
        Time (Duration (Result_Secs) +
              Duration_Adjust +
              (Seconds - Duration (Int_Secs)));

   end Time_Of;

   ----------
   -- Year --
   ----------

   function Year (Date : Time) return Year_Number is
      DY : Year_Number;
      DM : Month_Number;
      DD : Day_Number;
      DS : Day_Duration;

   begin
      Split (Date, DY, DM, DD, DS);
      return DY;
   end Year;

end Ada.Calendar;
