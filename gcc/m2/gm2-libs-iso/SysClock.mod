(* SysClock.mod implement the ISO SysClock specification.

Copyright (C) 2009-2023 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE SysClock ;

FROM wraptime IMPORT timeval, timezone, tm,
                     InitTimezone, InitTimeval,
                     InitTM, KillTM,
                     gettimeofday, settimeofday, GetFractions,
                     localtime_r, GetSummerTime, GetDST,
                     KillTimezone, KillTimeval, GetYear,
                     GetMonth, GetDay, GetHour, GetMinute,
                     GetSecond, SetTimeval, SetTimezone ;

IMPORT Args ;

VAR
   canget,
   canset,
   known : BOOLEAN ;


(*
   determineAccess - test to see whether we can get and set
                     the time.
*)

PROCEDURE determineAccess ;
VAR
   tv: timeval ;
   tz: timezone ;
BEGIN
   tz := InitTimezone () ;
   tv := InitTimeval () ;
   canget := gettimeofday (tv, tz) = 0 ;
   canset := canget AND (settimeofday (tv, tz) = 0) ;
   tz := KillTimezone (tz) ;
   tv := KillTimeval (tv)
END determineAccess ;


PROCEDURE CanGetClock () : BOOLEAN ;
(* Tests if the clock can be read *)
BEGIN
   IF NOT known
   THEN
      determineAccess
   END ;
   RETURN canget
END CanGetClock ;


PROCEDURE CanSetClock () : BOOLEAN ;
(* Tests if the clock can be set *)
BEGIN
   IF NOT known
   THEN
      determineAccess
   END ;
   RETURN canset
END CanSetClock ;


PROCEDURE IsValidDateTime (userData: DateTime) : BOOLEAN ;
(* Tests if the value of userData is a valid *)
BEGIN
   WITH userData DO
      CASE month OF

      1:  |
      2:  IF ((year MOD 4=0) AND (year MOD 100#0)) OR (year MOD 400=0)
          THEN
             RETURN day<=29
          ELSE
             RETURN day<=28
          END |
      3:  |
      4:  RETURN day<=30 |
      5:  |
      6:  RETURN day<=30 |
      7:  |
      8:  |
      9:  RETURN day<=30 |
      10: |
      11: RETURN day<=30 |
      12:

      END
   END ;
   RETURN( TRUE )
END IsValidDateTime ;


(*
   foo -
*)

PROCEDURE foo () : CARDINAL ;
BEGIN
   RETURN 1
END foo ;


PROCEDURE GetClock (VAR userData: DateTime) ;
(* Assigns local date and time of the day to userData *)
VAR
   m : tm ;
   tv: timeval ;
   tz: timezone ;
BEGIN
   IF CanGetClock ()
   THEN
      tv := InitTimeval () ;
      tz := InitTimezone () ;
      IF gettimeofday (tv, tz)=0
      THEN
         m := InitTM () ;
         (* m := localtime_r (tv, m) ; *)
         WITH userData DO
         (*
            year := GetYear (m) ;
         *)
            month := Args.Narg () (* GetMonth (m) *) (* + 1 *) ;
            (*
            day := GetDay (m) ;
            hour := GetHour (m) ;
            minute := GetMinute (m) ;
            second := GetSecond (m) ;
            fractions := GetFractions (tv) ;
            zone := GetDST (tz) ;
            summerTimeFlag := GetSummerTime (tz)
            *)
         END ;
         m := KillTM (m)
      ELSE
         HALT
      END ;
      tv := KillTimeval (tv) ;
      tz := KillTimezone (tz)
   END
END GetClock ;


(*
   daysInMonth - returns how many days there are in a month.
*)

PROCEDURE daysInMonth (year, month: CARDINAL) : CARDINAL ;
BEGIN
   CASE month OF

   1:  |
   2:  IF ((year MOD 4=0) AND (year MOD 100#0)) OR (year MOD 400=0)
       THEN
          RETURN 29
       ELSE
          RETURN 28
       END |
   3:  |
   4:  RETURN 30 |
   5:  |
   6:  RETURN 30 |
   7:  |
   8:  |
   9:  RETURN 30 |
   10: |
   11: RETURN 30 |
   12: |

   END ;
   RETURN 31
END daysInMonth ;


(*
   dayInYear -
*)

PROCEDURE dayInYear (day, month, year: CARDINAL) : CARDINAL ;
BEGIN
   WHILE month > 1 DO
      INC (day, daysInMonth (year, month)) ;
      DEC (month)
   END ;
   RETURN day
END dayInYear ;


(*
   dayInWeek -
*)

PROCEDURE dayInWeek (day, month, year: CARDINAL) : CARDINAL ;
CONST
   janFirst1970 = 5 ;   (* thursday *)
VAR
   yearOffset: CARDINAL ;  (* days since Jan 1st 1970 *)
BEGIN
   yearOffset := janFirst1970 ;
   WHILE year > 1970 DO
      DEC (year) ;
      INC (yearOffset, dayInYear (31, 12, year))
   END ;
   INC (yearOffset, dayInYear (day, month, year)) ;
   RETURN yearOffset MOD 7
END dayInWeek ;


PROCEDURE SetClock (userData: DateTime);
(* Sets the system time clock to the given local date and
   time *)
VAR
   tv: timeval ;
   tz: timezone ;
BEGIN
   IF CanSetClock ()
   THEN
      tv := InitTimeval () ;
      tz := InitTimezone () ;
      IF gettimeofday (tv, tz) = 0
      THEN
         (* fill in as many of tv, tz fields from userData as we can *)
         WITH userData DO
            IF summerTimeFlag
            THEN
               SetTimeval (tv, second, minute, hour, day, month, year,
                           dayInYear(day, month, year),
                           dayInWeek(day, month, year),
                           1) ;
               SetTimezone (tz, 1, zone)
            ELSE
               SetTimeval (tv, second, minute, hour, day, month, year,
                           dayInYear(day, month, year),
                           dayInWeek(day, month, year),
                           0) ;
               SetTimezone (tz, 0, zone)
            END ;
            IF settimeofday (tv, tz)#0
            THEN
               (* error, which we ignore *)
            END
         END
      END ;
      tv := KillTimeval (tv) ;
      tz := KillTimezone (tz)
   END
END SetClock ;


BEGIN
   known := FALSE ;
   canset := FALSE ;
   canget := FALSE
END SysClock.
