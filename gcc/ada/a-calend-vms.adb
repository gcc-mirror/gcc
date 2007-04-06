------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                         A D A . C A L E N D A R                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2006, Free Software Foundation, Inc.         --
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

--  This is the Alpha/VMS version

with System.Aux_DEC; use System.Aux_DEC;

with Ada.Unchecked_Conversion;

package body Ada.Calendar is

   --------------------------
   -- Implementation Notes --
   --------------------------

   --  Variables of type Ada.Calendar.Time have suffix _S or _M to denote
   --  units of seconds or milis.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function All_Leap_Seconds return Natural;
   --  Return the number of all leap seconds allocated so far

   procedure Cumulative_Leap_Seconds
     (Start_Date    : Time;
      End_Date      : Time;
      Elapsed_Leaps : out Natural;
      Next_Leap_Sec : out Time);
   --  Elapsed_Leaps is the sum of the leap seconds that have occured on or
   --  after Start_Date and before (strictly before) End_Date. Next_Leap_Sec
   --  represents the next leap second occurence on or after End_Date. If there
   --  are no leaps seconds after End_Date, After_Last_Leap is returned.
   --  After_Last_Leap can be used as End_Date to count all the leap seconds
   --  that have occured on or after Start_Date.
   --
   --  Note: Any sub seconds of Start_Date and End_Date are discarded before
   --  the calculations are done. For instance: if 113 seconds is a leap
   --  second (it isn't) and 113.5 is input as an End_Date, the leap second
   --  at 113 will not be counted in Leaps_Between, but it will be returned
   --  as Next_Leap_Sec. Thus, if the caller wants to know if the End_Date is
   --  a leap second, the comparison should be:
   --
   --     End_Date >= Next_Leap_Sec;
   --
   --  After_Last_Leap is designed so that this comparison works without
   --  having to first check if Next_Leap_Sec is a valid leap second.

   function To_Duration (T : Time) return Duration;
   function To_Relative_Time (D : Duration) return Time;
   --  It is important to note that duration's fractional part denotes nano
   --  seconds while the units of Time are 100 nanoseconds. If a regular
   --  Unchecked_Conversion was employed, the resulting values would be off
   --  by 100.

   ---------------------
   -- Local Constants --
   ---------------------

   After_Last_Leap : constant Time := Time'Last;
   N_Leap_Seconds  : constant Natural := 23;

   Cumulative_Days_Before_Month :
     constant array (Month_Number) of Natural :=
       (0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334);

   Leap_Second_Times : array (1 .. N_Leap_Seconds) of Time;
   --  Each value represents a time value which is one second before a leap
   --  second occurence. This table is populated during the elaboration of
   --  Ada.Calendar.

   ---------
   -- "+" --
   ---------

   function "+" (Left : Time; Right : Duration) return Time is
      pragma Unsuppress (Overflow_Check);

      Ada_High_And_Leaps : constant Time :=
                             Ada_High + Time (All_Leap_Seconds) * Mili;
      Result             : constant Time := Left + To_Relative_Time (Right);

   begin
      if Result < Ada_Low
        or else Result >= Ada_High_And_Leaps
      then
         raise Time_Error;
      end if;

      return Result;
   exception
      when Constraint_Error =>
         raise Time_Error;
   end "+";

   function "+" (Left : Duration; Right : Time) return Time is
      pragma Unsuppress (Overflow_Check);
   begin
      return Right + Left;
   exception
      when Constraint_Error =>
         raise Time_Error;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left : Time; Right : Duration) return Time is
      pragma Unsuppress (Overflow_Check);

      Ada_High_And_Leaps : constant Time :=
                             Ada_High + Time (All_Leap_Seconds) * Mili;
      Result             : constant Time := Left - To_Relative_Time (Right);

   begin
      if Result < Ada_Low
        or else Result >= Ada_High_And_Leaps
      then
         raise Time_Error;
      end if;

      return Result;

   exception
      when Constraint_Error =>
         raise Time_Error;
   end "-";

   function "-" (Left : Time; Right : Time) return Duration is
      pragma Unsuppress (Overflow_Check);

      Diff     : constant Time := Left - Right;
      Dur_High : constant Time := Time (Duration'Last) * 100;
      Dur_Low  : constant Time := Time (Duration'First) * 100;

   begin
      if Diff < Dur_Low
        or else Diff > Dur_High
      then
         raise Time_Error;
      end if;

      return To_Duration (Diff);

   exception
      when Constraint_Error =>
         raise Time_Error;
   end "-";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Time) return Boolean is
   begin
      return Long_Integer (Left) < Long_Integer (Right);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Time) return Boolean is
   begin
      return Long_Integer (Left) <= Long_Integer (Right);
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Time) return Boolean is
   begin
      return Long_Integer (Left) > Long_Integer (Right);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Time) return Boolean is
   begin
      return Long_Integer (Left) >= Long_Integer (Right);
   end ">=";

   ----------------------
   -- All_Leap_Seconds --
   ----------------------

   function All_Leap_Seconds return Natural is
   begin
      return N_Leap_Seconds;
   end All_Leap_Seconds;

   -----------
   -- Clock --
   -----------

   function Clock return Time is
      Elapsed_Leaps : Natural;
      Next_Leap     : Time;
      Now           : constant Time := Time (OSP.OS_Clock);
      Rounded_Now   : constant Time := Now - (Now mod Mili);

   begin
      --  Note that on other targets a soft-link is used to get a different
      --  clock depending whether tasking is used or not. On VMS this isn't
      --  needed since all clock calls end up using SYS$GETTIM, so call the
      --  OS_Primitives version for efficiency.

      --  Determine the number of leap seconds elapsed until this moment

      Cumulative_Leap_Seconds (Ada_Low, Now, Elapsed_Leaps, Next_Leap);

      --  It is possible that OS_Clock falls exactly on a leap second

      if Rounded_Now = Next_Leap then
         return Now + Time (Elapsed_Leaps + 1) * Mili;
      else
         return Now + Time (Elapsed_Leaps) * Mili;
      end if;
   end Clock;

   -----------------------------
   -- Cumulative_Leap_Seconds --
   -----------------------------

   procedure Cumulative_Leap_Seconds
     (Start_Date    : Time;
      End_Date      : Time;
      Elapsed_Leaps : out Natural;
      Next_Leap_Sec : out Time)
   is
      End_Index   : Positive;
      End_T       : Time := End_Date;
      Start_Index : Positive;
      Start_T     : Time := Start_Date;

   begin
      pragma Assert (Start_Date >= End_Date);

      Next_Leap_Sec := After_Last_Leap;

      --  Make sure that the end date does not excede the upper bound
      --  of Ada time.

      if End_Date > Ada_High then
         End_T := Ada_High;
      end if;

      --  Remove the sub seconds from both dates

      Start_T := Start_T - (Start_T mod Mili);
      End_T   := End_T   - (End_T   mod Mili);

      --  Some trivial cases

      if End_T < Leap_Second_Times (1) then
         Elapsed_Leaps := 0;
         Next_Leap_Sec := Leap_Second_Times (1);
         return;

      elsif Start_T > Leap_Second_Times (N_Leap_Seconds) then
         Elapsed_Leaps := 0;
         Next_Leap_Sec := After_Last_Leap;
         return;
      end if;

      --  Perform the calculations only if the start date is within the leap
      --  second occurences table.

      if Start_T <= Leap_Second_Times (N_Leap_Seconds) then

         --    1    2                  N - 1   N
         --  +----+----+--  . . .  --+-------+---+
         --  | T1 | T2 |             | N - 1 | N |
         --  +----+----+--  . . .  --+-------+---+
         --         ^                   ^
         --         | Start_Index       | End_Index
         --         +-------------------+
         --             Leaps_Between

         --  The idea behind the algorithm is to iterate and find two closest
         --  dates which are after Start_T and End_T. Their corresponding index
         --  difference denotes the number of leap seconds elapsed.

         Start_Index := 1;
         loop
            exit when Leap_Second_Times (Start_Index) >= Start_T;
            Start_Index := Start_Index + 1;
         end loop;

         End_Index := Start_Index;
         loop
            exit when End_Index > N_Leap_Seconds
              or else Leap_Second_Times (End_Index) >= End_T;
            End_Index := End_Index + 1;
         end loop;

         if End_Index <= N_Leap_Seconds then
            Next_Leap_Sec := Leap_Second_Times (End_Index);
         end if;

         Elapsed_Leaps := End_Index - Start_Index;

      else
         Elapsed_Leaps := 0;
      end if;
   end Cumulative_Leap_Seconds;

   ---------
   -- Day --
   ---------

   function Day (Date : Time) return Day_Number is
      Y : Year_Number;
      M : Month_Number;
      D : Day_Number;
      S : Day_Duration;
   begin
      Split (Date, Y, M, D, S);
      return D;
   end Day;

   -------------
   -- Is_Leap --
   -------------

   function Is_Leap (Year : Year_Number) return Boolean is
   begin
      --  Leap centenial years

      if Year mod 400 = 0 then
         return True;

      --  Non-leap centenial years

      elsif Year mod 100 = 0 then
         return False;

      --  Regular years

      else
         return Year mod 4 = 0;
      end if;
   end Is_Leap;

   -----------
   -- Month --
   -----------

   function Month (Date : Time) return Month_Number is
      Y : Year_Number;
      M : Month_Number;
      D : Day_Number;
      S : Day_Duration;
   begin
      Split (Date, Y, M, D, S);
      return M;
   end Month;

   -------------
   -- Seconds --
   -------------

   function Seconds (Date : Time) return Day_Duration is
      Y : Year_Number;
      M : Month_Number;
      D : Day_Number;
      S : Day_Duration;
   begin
      Split (Date, Y, M, D, S);
      return S;
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
      H  : Integer;
      M  : Integer;
      Se : Integer;
      Ss : Duration;
      Le : Boolean;

   begin
      Formatting_Operations.Split
        (Date, Year, Month, Day, Seconds, H, M, Se, Ss, Le, 0);

      --  Validity checks

      if not Year'Valid
        or else not Month'Valid
        or else not Day'Valid
        or else not Seconds'Valid
      then
         raise Time_Error;
      end if;
   end Split;

   -------------
   -- Time_Of --
   -------------

   function Time_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration := 0.0) return Time
   is
      --  The values in the following constants are irrelevant, they are just
      --  placeholders; the choice of constructing a Day_Duration value is
      --  controlled by the Use_Day_Secs flag.

      H  : constant Integer := 1;
      M  : constant Integer := 1;
      Se : constant Integer := 1;
      Ss : constant Duration := 0.1;

   begin
      if not Year'Valid
        or else not Month'Valid
        or else not Day'Valid
        or else not Seconds'Valid
      then
         raise Time_Error;
      end if;

      return
        Formatting_Operations.Time_Of
          (Year, Month, Day, Seconds, H, M, Se, Ss,
           Leap_Sec     => False,
           Leap_Checks  => False,
           Use_Day_Secs => True,
           Time_Zone    => 0);
   end Time_Of;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (T : Time) return Duration is
      function Time_To_Duration is
        new Ada.Unchecked_Conversion (Time, Duration);
   begin
      return Time_To_Duration (T * 100);
   end To_Duration;

   ----------------------
   -- To_Relative_Time --
   ----------------------

   function To_Relative_Time (D : Duration) return Time is
      function Duration_To_Time is
        new Ada.Unchecked_Conversion (Duration, Time);
   begin
      return Duration_To_Time (D / 100.0);
   end To_Relative_Time;

   ----------
   -- Year --
   ----------

   function Year (Date : Time) return Year_Number is
      Y : Year_Number;
      M : Month_Number;
      D : Day_Number;
      S : Day_Duration;
   begin
      Split (Date, Y, M, D, S);
      return Y;
   end Year;

   --  The following packages assume that Time is a Long_Integer, the units
   --  are 100 nanoseconds and the starting point in the VMS Epoch.

   ---------------------------
   -- Arithmetic_Operations --
   ---------------------------

   package body Arithmetic_Operations is

      ---------
      -- Add --
      ---------

      function Add (Date : Time; Days : Long_Integer) return Time is
         Ada_High_And_Leaps : constant Time :=
                                Ada_High + Time (All_Leap_Seconds) * Mili;
      begin
         if Days = 0 then
            return Date;

         elsif Days < 0 then
            return Subtract (Date, abs (Days));

         else
            declare
               Result : constant Time := Date + Time (Days) * Milis_In_Day;

            begin
               --  The result excedes the upper bound of Ada time

               if Result >= Ada_High_And_Leaps then
                  raise Time_Error;
               end if;

               return Result;
            end;
         end if;

      exception
         when Constraint_Error =>
            raise Time_Error;
      end Add;

      ----------------
      -- Difference --
      ----------------

      procedure Difference
        (Left         : Time;
         Right        : Time;
         Days         : out Long_Integer;
         Seconds      : out Duration;
         Leap_Seconds : out Integer)
      is
         Mili_F : constant Duration := 10_000_000.0;

         Diff_M        : Time;
         Diff_S        : Time;
         Earlier       : Time;
         Elapsed_Leaps : Natural;
         Later         : Time;
         Negate        : Boolean;
         Next_Leap     : Time;
         Sub_Seconds   : Duration;

      begin
         --  This classification is necessary in order to avoid a Time_Error
         --  being raised by the arithmetic operators in Ada.Calendar.

         if Left >= Right then
            Later   := Left;
            Earlier := Right;
            Negate  := False;
         else
            Later   := Right;
            Earlier := Left;
            Negate  := True;
         end if;

         --  First process the leap seconds

         Cumulative_Leap_Seconds (Earlier, Later, Elapsed_Leaps, Next_Leap);

         if Later >= Next_Leap then
            Elapsed_Leaps := Elapsed_Leaps + 1;
         end if;

         Diff_M := Later - Earlier - Time (Elapsed_Leaps) * Mili;

         --  Sub second processing

         Sub_Seconds := Duration (Diff_M mod Mili) / Mili_F;

         --  Convert to seconds. Note that his action eliminates the sub
         --  seconds automatically.

         Diff_S := Diff_M / Mili;

         Days := Long_Integer (Diff_S / Secs_In_Day);
         Seconds := Duration (Diff_S mod Secs_In_Day) + Sub_Seconds;
         Leap_Seconds := Integer (Elapsed_Leaps);

         if Negate then
            Days         := -Days;
            Seconds      := -Seconds;
            Leap_Seconds := -Leap_Seconds;
         end if;
      end Difference;

      --------------
      -- Subtract --
      --------------

      function Subtract (Date : Time; Days : Long_Integer) return Time is
      begin
         if Days = 0 then
            return Date;

         elsif Days < 0 then
            return Add (Date, abs (Days));

         else
            declare
               Days_T : constant Time := Time (Days) * Milis_In_Day;
               Result : constant Time := Date - Days_T;

            begin
               --  Subtracting a larger number of days from a smaller time
               --  value will cause wrap around since time is a modular type.
               --  Also the result may be lower than the start of Ada time.

               if Date < Days_T
                 or Result < Ada_Low
               then
                  raise Time_Error;
               end if;

               return Date - Days_T;
            end;
         end if;
      exception
         when Constraint_Error =>
            raise Time_Error;
      end Subtract;
   end Arithmetic_Operations;

   ---------------------------
   -- Formatting_Operations --
   ---------------------------

   package body Formatting_Operations is

      -----------------
      -- Day_Of_Week --
      -----------------

      function Day_Of_Week (Date : Time) return Integer is
         Y : Year_Number;
         M : Month_Number;
         D : Day_Number;
         S : Day_Duration;

         Day_Count     : Long_Integer;
         Midday_Date_S : Time;

      begin
         Split (Date, Y, M, D, S);

         --  Build a time value in the middle of the same day and convert the
         --  time value to seconds.

         Midday_Date_S := Time_Of (Y, M, D, 43_200.0) / Mili;

         --  Count the number of days since the start of VMS time. 1858-11-17
         --  was a Wednesday.

         Day_Count := Long_Integer (Midday_Date_S / Secs_In_Day) + 2;

         return Integer (Day_Count mod 7);
      end Day_Of_Week;

      -----------
      -- Split --
      -----------

      procedure Split
        (Date         : Time;
         Year         : out Year_Number;
         Month        : out Month_Number;
         Day          : out Day_Number;
         Day_Secs     : out Day_Duration;
         Hour         : out Integer;
         Minute       : out Integer;
         Second       : out Integer;
         Sub_Sec      : out Duration;
         Leap_Sec     : out Boolean;
         Time_Zone    : Long_Integer)
      is
         procedure Numtim
           (Status : out Unsigned_Longword;
            Timbuf : out Unsigned_Word_Array;
            Timadr : Time);

         pragma Interface (External, Numtim);

         pragma Import_Valued_Procedure
           (Numtim, "SYS$NUMTIM",
           (Unsigned_Longword, Unsigned_Word_Array, Time),
           (Value, Reference, Reference));

         Status : Unsigned_Longword;
         Timbuf : Unsigned_Word_Array (1 .. 7);

         Ada_Min_Year : constant := 1901;
         Ada_Max_Year : constant := 2399;
         Mili_F       : constant Duration := 10_000_000.0;

         Abs_Time_Zone   : Time;
         Elapsed_Leaps   : Natural;
         Modified_Date_M : Time;
         Next_Leap_M     : Time;
         Rounded_Date_M  : Time;

      begin
         Modified_Date_M := Date;

         --  Step 1: Leap seconds processing

         Cumulative_Leap_Seconds (Ada_Low, Date, Elapsed_Leaps, Next_Leap_M);

         Rounded_Date_M  := Modified_Date_M - (Modified_Date_M mod Mili);
         Leap_Sec        := Rounded_Date_M = Next_Leap_M;
         Modified_Date_M := Modified_Date_M - Time (Elapsed_Leaps) * Mili;

         if Leap_Sec then
            Modified_Date_M := Modified_Date_M - Time (1) * Mili;
         end if;

         --  Step 2: Time zone processing

         if Time_Zone /= 0 then
            Abs_Time_Zone := Time (abs (Time_Zone)) * 60 * Mili;

            if Time_Zone < 0 then
               Modified_Date_M := Modified_Date_M - Abs_Time_Zone;
            else
               Modified_Date_M := Modified_Date_M + Abs_Time_Zone;
            end if;
         end if;

         --  After the leap seconds and time zone have been accounted for,
         --  the date should be within the bounds of Ada time.

         if Modified_Date_M < Ada_Low
           or else Modified_Date_M >= Ada_High
         then
            raise Time_Error;
         end if;

         --  Step 3: Sub second processing

         Sub_Sec := Duration (Modified_Date_M mod Mili) / Mili_F;

         --  Drop the sub seconds

         Modified_Date_M := Modified_Date_M - (Modified_Date_M mod Mili);

         --  Step 4: VMS system call

         Numtim (Status, Timbuf, Modified_Date_M);

         if Status mod 2 /= 1
           or else Timbuf (1) not in Ada_Min_Year .. Ada_Max_Year
         then
            raise Time_Error;
         end if;

         --  Step 5: Time components processing

         Year   := Year_Number (Timbuf (1));
         Month  := Month_Number (Timbuf (2));
         Day    := Day_Number (Timbuf (3));
         Hour   := Integer (Timbuf (4));
         Minute := Integer (Timbuf (5));
         Second := Integer (Timbuf (6));

         Day_Secs := Day_Duration (Hour   * 3_600) +
                     Day_Duration (Minute *    60) +
                     Day_Duration (Second)         +
                                   Sub_Sec;
      end Split;

      -------------
      -- Time_Of --
      -------------

      function Time_Of
        (Year         : Year_Number;
         Month        : Month_Number;
         Day          : Day_Number;
         Day_Secs     : Day_Duration;
         Hour         : Integer;
         Minute       : Integer;
         Second       : Integer;
         Sub_Sec      : Duration;
         Leap_Sec     : Boolean;
         Leap_Checks  : Boolean;
         Use_Day_Secs : Boolean;
         Time_Zone    : Long_Integer) return Time
      is
         procedure Cvt_Vectim
           (Status         : out Unsigned_Longword;
            Input_Time     : Unsigned_Word_Array;
            Resultant_Time : out Time);

         pragma Interface (External, Cvt_Vectim);

         pragma Import_Valued_Procedure
           (Cvt_Vectim, "LIB$CVT_VECTIM",
           (Unsigned_Longword, Unsigned_Word_Array, Time),
           (Value, Reference, Reference));

         Status : Unsigned_Longword;
         Timbuf : Unsigned_Word_Array (1 .. 7);

         Mili_F : constant := 10_000_000.0;

         Ada_High_And_Leaps : constant Time :=
                                Ada_High + Time (All_Leap_Seconds) * Mili;

         H  : Integer  := Hour;
         Mi : Integer  := Minute;
         Se : Integer  := Second;
         Su : Duration := Sub_Sec;

         Abs_Time_Zone    : Time;
         Adjust_Day       : Boolean := False;
         Elapsed_Leaps    : Natural;
         Int_Day_Secs     : Integer;
         Next_Leap_M      : Time;
         Result_M         : Time;
         Rounded_Result_M : Time;

      begin
         --  No validity checks are performed on the input values since it is
         --  assumed that the called has already performed them.

         --  Step 1: Hour, minute, second and sub second processing

         if Use_Day_Secs then

            --  A day seconds value of 86_400 designates a new day. The time
            --  components are reset to zero, but an additional day will be
            --  added after the system call.

            if Day_Secs = 86_400.0 then
               Adjust_Day := True;
               H  := 0;
               Mi := 0;
               Se := 0;

            else
               --  Sub second extraction

               if Day_Secs > 0.0 then
                  Int_Day_Secs := Integer (Day_Secs - 0.5);
               else
                  Int_Day_Secs := Integer (Day_Secs);
               end if;

               H  := Int_Day_Secs / 3_600;
               Mi := (Int_Day_Secs / 60) mod 60;
               Se := Int_Day_Secs mod 60;
               Su := Day_Secs - Duration (Int_Day_Secs);
            end if;
         end if;

         --  Step 2: System call to VMS

         Timbuf (1) := Unsigned_Word (Year);
         Timbuf (2) := Unsigned_Word (Month);
         Timbuf (3) := Unsigned_Word (Day);
         Timbuf (4) := Unsigned_Word (H);
         Timbuf (5) := Unsigned_Word (Mi);
         Timbuf (6) := Unsigned_Word (Se);
         Timbuf (7) := 0;

         Cvt_Vectim (Status, Timbuf, Result_M);

         if Status mod 2 /= 1 then
            raise Time_Error;
         end if;

         --  Step 3: Potential day adjustment

         if Use_Day_Secs
           and then Adjust_Day
         then
            Result_M := Result_M + Milis_In_Day;
         end if;

         --  Step 4: Sub second adjustment

         Result_M := Result_M + Time (Su * Mili_F);

         --  Step 5: Time zone processing

         if Time_Zone /= 0 then
            Abs_Time_Zone := Time (abs (Time_Zone)) * 60 * Mili;

            if Time_Zone < 0 then
               Result_M := Result_M + Abs_Time_Zone;
            else
               Result_M := Result_M - Abs_Time_Zone;
            end if;
         end if;

         --  Step 6: Leap seconds processing

         Cumulative_Leap_Seconds
           (Ada_Low, Result_M, Elapsed_Leaps, Next_Leap_M);

         Result_M := Result_M + Time (Elapsed_Leaps) * Mili;

         --  An Ada 2005 caller requesting an explicit leap second or an Ada
         --  95 caller accounting for an invisible leap second.

         Rounded_Result_M := Result_M - (Result_M mod Mili);

         if Leap_Sec
           or else Rounded_Result_M = Next_Leap_M
         then
            Result_M := Result_M + Time (1) * Mili;
            Rounded_Result_M := Rounded_Result_M + Time (1) * Mili;
         end if;

         --  Leap second validity check

         if Leap_Checks
           and then Leap_Sec
           and then Rounded_Result_M /= Next_Leap_M
         then
            raise Time_Error;
         end if;

         --  Bounds check

         if Result_M < Ada_Low
           or else Result_M >= Ada_High_And_Leaps
         then
            raise Time_Error;
         end if;

         return Result_M;
      end Time_Of;
   end Formatting_Operations;

   ---------------------------
   -- Time_Zones_Operations --
   ---------------------------

   package body Time_Zones_Operations is

      ---------------------
      -- UTC_Time_Offset --
      ---------------------

      function UTC_Time_Offset (Date : Time) return Long_Integer is
         --  Formal parameter Date is here for interfacing, but is never
         --  actually used.

         pragma Unreferenced (Date);

         function get_gmtoff return Long_Integer;
         pragma Import (C, get_gmtoff, "get_gmtoff");

      begin
         --  VMS is not capable of determining the time zone in some past or
         --  future point in time denoted by Date, thus the current time zone
         --  is retrieved.

         return get_gmtoff;
      end UTC_Time_Offset;
   end Time_Zones_Operations;

--  Start of elaboration code for Ada.Calendar

begin
   --  Population of the leap seconds table

   declare
      type Leap_Second_Date is record
         Year  : Year_Number;
         Month : Month_Number;
         Day   : Day_Number;
      end record;

      Leap_Second_Dates :
        constant array (1 .. N_Leap_Seconds) of Leap_Second_Date :=
          ((1972,  6, 30), (1972, 12, 31), (1973, 12, 31), (1974, 12, 31),
           (1975, 12, 31), (1976, 12, 31), (1977, 12, 31), (1978, 12, 31),
           (1979, 12, 31), (1981,  6, 30), (1982,  6, 30), (1983,  6, 30),
           (1985,  6, 30), (1987, 12, 31), (1989, 12, 31), (1990, 12, 31),
           (1992,  6, 30), (1993,  6, 30), (1994,  6, 30), (1995, 12, 31),
           (1997,  6, 30), (1998, 12, 31), (2005, 12, 31));

      Ada_Min_Year       : constant Year_Number := Year_Number'First;
      Days_In_Four_Years : constant := 365 * 3 + 366;
      VMS_Days           : constant := 10 * 366 + 32 * 365 + 45;

      Days  : Natural;
      Leap  : Leap_Second_Date;
      Years : Natural;

   begin
      for Index in 1 .. N_Leap_Seconds loop
         Leap := Leap_Second_Dates (Index);

         --  Calculate the number of days from the start of Ada time until
         --  the current leap second occurence. Non-leap centenial years
         --  are not accounted for in these calculations since there are
         --  no leap seconds after 2100 yet.

         Years := Leap.Year - Ada_Min_Year;
         Days  := (Years / 4) * Days_In_Four_Years;
         Years := Years mod 4;

         if Years = 1 then
            Days := Days + 365;

         elsif Years = 2 then
            Days := Days + 365 * 2;

         elsif Years = 3 then
            Days := Days + 365 * 3;
         end if;

         Days := Days + Cumulative_Days_Before_Month (Leap.Month);

         if Is_Leap (Leap.Year)
           and then Leap.Month > 2
         then
            Days := Days + 1;
         end if;

         --  Add the number of days since the start of VMS time till the
         --  start of Ada time.

         Days := Days + Leap.Day + VMS_Days;

         --  Index - 1 previous leap seconds are added to Time (Index)

         Leap_Second_Times (Index) :=
           (Time (Days) * Secs_In_Day + Time (Index - 1)) * Mili;
      end loop;
   end;

end Ada.Calendar;
