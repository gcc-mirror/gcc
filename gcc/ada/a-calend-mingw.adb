------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                         A D A . C A L E N D A R                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 1997-2002 Free Software Foundation, Inc.        --
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

--  This is the Windows NT/95 version.

with System.OS_Primitives;
--  used for Clock

with System.OS_Interface;

package body Ada.Calendar is

   use System.OS_Interface;

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

   Ada_Year_Min : constant := 1901;
   Ada_Year_Max : constant := 2099;

   --  Win32 time constants

   epoch_1970     : constant := 16#19D_B1DE_D53E_8000#; -- win32 UTC epoch
   system_time_ns : constant := 100;                    -- 100 ns per tick
   Sec_Unit       : constant := 10#1#E9;

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

   --  The Ada.Calendar.Clock function gets the time from the soft links
   --  interface which will call the appropriate function depending wether
   --  tasking is involved or not.

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

      Date_Int    : aliased Long_Long_Integer;
      Date_Loc    : aliased Long_Long_Integer;
      Timbuf      : aliased SYSTEMTIME;
      Int_Date    : Long_Long_Integer;
      Sub_Seconds : Duration;

   begin
      --  We take the sub-seconds (decimal part) of Date and this is added
      --  to compute the Seconds. This way we keep the precision of the
      --  high-precision clock that was lost with the Win32 API calls
      --  below.

      if Date < 0.0 then

         --  this is a Date before Epoch (January 1st, 1970)

         Sub_Seconds := Duration (Date) -
           Duration (Long_Long_Integer (Date + Duration'(0.5)));

         Int_Date := Long_Long_Integer (Date - Sub_Seconds);

         --  For Date = -86400.1 we are 2 days before Epoch at 0.1 seconds
         --  from day 1 before Epoch. It means that it is 23h 59m 59.9s.
         --  here we adjust for that.

         if Sub_Seconds < 0.0 then
            Int_Date    := Int_Date - 1;
            Sub_Seconds := 1.0 + Sub_Seconds;
         end if;

      else

         --  this is a Date after Epoch (January 1st, 1970)

         Sub_Seconds := Duration (Date) -
           Duration (Long_Long_Integer (Date - Duration'(0.5)));

         Int_Date := Long_Long_Integer (Date - Sub_Seconds);

      end if;

      --  Date_Int is the number of seconds from Epoch.

      Date_Int := Long_Long_Integer
        (Int_Date * Sec_Unit / system_time_ns) + epoch_1970;

      if not FileTimeToLocalFileTime (Date_Int'Access, Date_Loc'Access) then
         raise Time_Error;
      end if;

      if not FileTimeToSystemTime (Date_Loc'Access, Timbuf'Access) then
         raise Time_Error;
      end if;

      if Timbuf.wYear not in Ada_Year_Min .. Ada_Year_Max then
         raise Time_Error;
      end if;

      Seconds :=
        Duration (Timbuf.wHour) * 3_600.0 +
        Duration (Timbuf.wMinute) * 60.0 +
        Duration (Timbuf.wSecond) +
        Sub_Seconds;

      Day       := Integer (Timbuf.wDay);
      Month     := Integer (Timbuf.wMonth);
      Year      := Integer (Timbuf.wYear);
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

      Timbuf      : aliased SYSTEMTIME;
      Now         : aliased Long_Long_Integer;
      Loc         : aliased Long_Long_Integer;
      Int_Secs    : Integer;
      Secs        : Integer;
      Add_One_Day : Boolean := False;
      Date        : Time;

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

      if Seconds = 0.0 then
         Int_Secs := 0;
      else
         Int_Secs := Integer (Seconds - 0.5);
      end if;

      --  Timbuf.wMillisec is to keep the msec. We can't use that because the
      --  high-resolution clock has a precision of 1 Microsecond.
      --  Anyway the sub-seconds part is not needed to compute the number
      --  of seconds in UTC.

      if Int_Secs = 86_400 then
         Secs := 0;
         Add_One_Day := True;
      else
         Secs := Int_Secs;
      end if;

      Timbuf.wMilliseconds := 0;
      Timbuf.wSecond       := WORD (Secs mod 60);
      Timbuf.wMinute       := WORD ((Secs / 60) mod 60);
      Timbuf.wHour         := WORD (Secs / 3600);
      Timbuf.wDay          := WORD (Day);
      Timbuf.wMonth        := WORD (Month);
      Timbuf.wYear         := WORD (Year);

      if not SystemTimeToFileTime (Timbuf'Access, Loc'Access) then
         raise Time_Error;
      end if;

      if not LocalFileTimeToFileTime (Loc'Access, Now'Access) then
         raise Time_Error;
      end if;

      --  Here we have the UTC now translate UTC to Epoch time (UNIX style
      --  time based on 1 january 1970) and add there the sub-seconds part.

      declare
         Sub_Sec : constant Duration := Seconds - Duration (Int_Secs);
      begin
         Date := Time ((Now - epoch_1970) * system_time_ns / Sec_Unit) +
           Sub_Sec;
      end;

      if Add_One_Day then
         Date := Date + Duration (86400.0);
      end if;

      return Date;
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
