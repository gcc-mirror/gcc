------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         G N A T . C A L E N D A R                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.8 $
--                                                                          --
--           Copyright (C) 1999-2001 Ada Core Technologies, Inc.            --
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
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

package body GNAT.Calendar is

   use Ada.Calendar;
   use Interfaces;

   -----------------
   -- Day_In_Year --
   -----------------

   function Day_In_Year (Date : Time) return Day_In_Year_Number is
      Year  : Year_Number;
      Month : Month_Number;
      Day   : Day_Number;
      Dsecs : Day_Duration;

   begin
      Split (Date, Year, Month, Day, Dsecs);

      return Julian_Day (Year, Month, Day) - Julian_Day (Year, 1, 1) + 1;
   end Day_In_Year;

   -----------------
   -- Day_Of_Week --
   -----------------

   function Day_Of_Week (Date : Time) return Day_Name is
      Year  : Year_Number;
      Month : Month_Number;
      Day   : Day_Number;
      Dsecs : Day_Duration;

   begin
      Split (Date, Year, Month, Day, Dsecs);

      return Day_Name'Val ((Julian_Day (Year, Month, Day)) mod 7);
   end Day_Of_Week;

   ----------
   -- Hour --
   ----------

   function Hour (Date : Time) return Hour_Number is
      Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration;

   begin
      Split (Date, Year, Month, Day, Hour, Minute, Second, Sub_Second);
      return Hour;
   end Hour;

   ----------------
   -- Julian_Day --
   ----------------

   --  Julian_Day is used to by Day_Of_Week and Day_In_Year. Note
   --  that this implementation is not expensive.

   function Julian_Day
     (Year  : Year_Number;
      Month : Month_Number;
      Day   : Day_Number)
      return  Integer
   is
      Internal_Year  : Integer;
      Internal_Month : Integer;
      Internal_Day   : Integer;
      Julian_Date    : Integer;
      C              : Integer;
      Ya             : Integer;

   begin
      Internal_Year  := Integer (Year);
      Internal_Month := Integer (Month);
      Internal_Day   := Integer (Day);

      if Internal_Month > 2 then
         Internal_Month := Internal_Month - 3;
      else
         Internal_Month := Internal_Month + 9;
         Internal_Year  := Internal_Year - 1;
      end if;

      C  := Internal_Year / 100;
      Ya := Internal_Year - (100 * C);

      Julian_Date := (146_097 * C) / 4 +
        (1_461 * Ya) / 4 +
        (153 * Internal_Month + 2) / 5 +
        Internal_Day + 1_721_119;

      return Julian_Date;
   end Julian_Day;

   ------------
   -- Minute --
   ------------

   function Minute (Date : Time) return Minute_Number is
      Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration;

   begin
      Split (Date, Year, Month, Day, Hour, Minute, Second, Sub_Second);
      return Minute;
   end Minute;

   ------------
   -- Second --
   ------------

   function Second (Date : Time) return Second_Number is
      Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration;

   begin
      Split (Date, Year, Month, Day, Hour, Minute, Second, Sub_Second);
      return Second;
   end Second;

   -----------
   -- Split --
   -----------

   procedure Split
     (Date       : Time;
      Year       : out Year_Number;
      Month      : out Month_Number;
      Day        : out Day_Number;
      Hour       : out Hour_Number;
      Minute     : out Minute_Number;
      Second     : out Second_Number;
      Sub_Second : out Second_Duration)
   is
      Dsecs : Day_Duration;
      Secs  : Natural;

   begin
      Split (Date, Year, Month, Day, Dsecs);

      if Dsecs = 0.0 then
         Secs := 0;
      else
         Secs := Natural (Dsecs - 0.5);
      end if;

      Sub_Second := Second_Duration (Dsecs - Day_Duration (Secs));
      Hour       := Hour_Number (Secs / 3600);
      Secs       := Secs mod 3600;
      Minute     := Minute_Number (Secs / 60);
      Second     := Second_Number (Secs mod 60);
   end Split;

   ----------------
   -- Sub_Second --
   ----------------

   function Sub_Second (Date : Time) return Second_Duration is
      Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration;

   begin
      Split (Date, Year, Month, Day, Hour, Minute, Second, Sub_Second);
      return Sub_Second;
   end Sub_Second;

   -------------
   -- Time_Of --
   -------------

   function Time_Of
     (Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration := 0.0)
      return Time
   is
      Dsecs : constant Day_Duration :=
                Day_Duration (Hour * 3600 + Minute * 60 + Second) +
                                                             Sub_Second;
   begin
      return Time_Of (Year, Month, Day, Dsecs);
   end Time_Of;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (T : access timeval) return Duration is

      procedure timeval_to_duration
        (T    : access timeval;
         sec  : access C.long;
         usec : access C.long);
      pragma Import (C, timeval_to_duration, "__gnat_timeval_to_duration");

      Micro : constant := 10**6;
      sec   : aliased C.long;
      usec  : aliased C.long;


   begin
      timeval_to_duration (T, sec'Access, usec'Access);
      return Duration (sec) + Duration (usec) / Micro;
   end To_Duration;

   ----------------
   -- To_Timeval --
   ----------------

   function To_Timeval  (D : Duration) return timeval is

      procedure duration_to_timeval (Sec, Usec : C.long; T : access timeval);
      pragma Import (C, duration_to_timeval, "__gnat_duration_to_timeval");

      Micro  : constant := 10**6;
      Result : aliased timeval;
      sec    : C.long;
      usec   : C.long;

   begin
      if D = 0.0 then
         sec  := 0;
         usec := 0;
      else
         sec  := C.long (D - 0.5);
         usec := C.long ((D - Duration (sec)) * Micro - 0.5);
      end if;

      duration_to_timeval (sec, usec, Result'Access);

      return Result;
   end To_Timeval;

   ------------------
   -- Week_In_Year --
   ------------------

   function Week_In_Year
     (Date : Ada.Calendar.Time)
      return Week_In_Year_Number
   is
      Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration;
      Offset     : Natural;

   begin
      Split (Date, Year, Month, Day, Hour, Minute, Second, Sub_Second);

      --  Day offset number for the first week of the year.

      Offset := Julian_Day (Year, 1, 1) mod 7;

      return 1 + ((Day_In_Year (Date) - 1) + Offset) / 7;
   end Week_In_Year;

end GNAT.Calendar;
