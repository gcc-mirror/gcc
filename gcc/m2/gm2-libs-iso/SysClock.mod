(* SysClock.mod implement the ISO SysClock specification.

Copyright (C) 2009-2025 Free Software Foundation, Inc.
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

FROM wrapclock IMPORT timespec, timezone, isdst, InitTimespec, KillTimespec,
                      GetTimespec, SetTimespec, GetTimeRealtime, SetTimeRealtime ;

FROM libc IMPORT printf ;

IMPORT Args ;

CONST
   Debugging = FALSE ;

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
   ts: timespec ;
BEGIN
   IF NOT known
   THEN
      ts := InitTimespec () ;
      canget := GetTimeRealtime (ts) = 0 ;
      canset := canget AND (SetTimeRealtime (ts) = 0) ;
      ts := KillTimespec (ts) ;
      known := TRUE
   END
END determineAccess ;


PROCEDURE CanGetClock () : BOOLEAN ;
(* Tests if the clock can be read *)
BEGIN
   determineAccess ;
   RETURN canget
END CanGetClock ;


PROCEDURE CanSetClock () : BOOLEAN ;
(* Tests if the clock can be set *)
BEGIN
   determineAccess ;
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
   DivMod - returns seconds MOD modulus.  It also divides seconds by modulus.
*)

PROCEDURE DivMod (VAR seconds: LONGCARD; modulus: LONGCARD) : LONGCARD ;
VAR
   result: LONGCARD ;
BEGIN
   result := seconds MOD modulus ;
   seconds := seconds DIV modulus ;
   RETURN result
END DivMod ;


(*
   daysInYear - return the number of days in year up to month/day.
*)

PROCEDURE daysInYear (day, month, year: LONGCARD) : LONGCARD ;
BEGIN
   WHILE month > 1 DO
      INC (day, daysInMonth (year, month)) ;
      DEC (month)
   END ;
   RETURN day
END daysInYear ;


(*
   ExtractDate - extracts the year, month, day from secs.  days is the
                 total days since 1970.
*)

PROCEDURE ExtractDate (days: LONGCARD;
                       VAR year: CARDINAL; VAR month: Month; VAR day: Day) ;
VAR
   testMonth,
   testYear : CARDINAL ;
   monthOfDays,
   yearOfDays : LONGCARD ;
BEGIN
   testYear := 1970 ;
   LOOP
      yearOfDays := daysInYear (31, 12, testYear) ;
      IF days < yearOfDays
      THEN
         year := testYear ;
         testMonth := 1 ;
         LOOP
            monthOfDays := daysInMonth (year, testMonth) ;
            IF days < monthOfDays
            THEN
               day := VAL (Day, days) + MIN (Day) ;
               month := VAL (Month, testMonth) ;
               RETURN
            END ;
            DEC (days, monthOfDays) ;
            INC (testMonth)
         END
      ELSE
         DEC (days, yearOfDays) ;
         INC (testYear)
      END
   END
END ExtractDate ;


(*
   EpochTime - assigns all fields of userData to 0 or FALSE.
*)

PROCEDURE EpochTime (VAR userData: DateTime) ;
BEGIN
   WITH userData DO
      second := 0 ;
      minute :=  0 ;
      hour := 0 ;
      year := 0 ;
      month := 0 ;
      day := 0 ;
      fractions := 0 ;
      zone := 0 ;
      summerTimeFlag := FALSE
   END
END EpochTime ;


PROCEDURE GetClock (VAR userData: DateTime) ;
(* Assigns local date and time of the day to userData *)
VAR
   ts       : timespec ;
   nano, sec: LONGCARD ;
   offset   : LONGINT ;
BEGIN
   IF CanGetClock ()
   THEN
      ts := InitTimespec () ;
      IF GetTimeRealtime (ts) = 0
      THEN
         IF GetTimespec (ts, sec, nano) = 1
         THEN
            offset := timezone () ;
            IF Debugging
            THEN
               printf ("getclock = %ld\n", sec)
            END ;
            sec := VAL (LONGINT, sec) + offset ;
            IF Debugging
            THEN
               printf ("getclock = %ld\n", sec)
            END ;
            WITH userData DO
               (* Here we keep dividing sec by max seconds, minutes, hours
                  to convert sec into total days since epoch.  *)
               second := VAL (Sec, DivMod (sec, MAX (Sec) + 1)) ;
               minute := VAL (Min, DivMod (sec, MAX (Min) + 1)) ;
               hour := VAL (Hour, DivMod (sec, MAX (Hour) + 1)) ;
               ExtractDate (sec, year, month, day) ;
               fractions := nano DIV ((1000 * 1000 * 1000) DIV maxSecondParts) ;
               zone := - (offset DIV 60) ;
               summerTimeFlag := (isdst () = 1)
            END
         ELSE
            EpochTime (userData)
         END
      ELSE
         EpochTime (userData)
      END ;
      ts := KillTimespec (ts)
   END
END GetClock ;


(*
   daysInMonth - returns how many days there are in a month.
*)

PROCEDURE daysInMonth (year, month: CARDINAL) : LONGCARD ;
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
   totalYear - return the sum of all days prior to year from the epoch.
*)

PROCEDURE totalYear (year: LONGCARD) : LONGCARD ;
VAR
   lastYear,
   result  : LONGCARD ;
BEGIN
   lastYear := 1970 ;
   result := 0 ;
   WHILE lastYear < year DO
      INC (result, daysInYear (31, 12, lastYear)) ;
      INC (lastYear)
   END ;
   RETURN result
END totalYear ;


(*
   totalSeconds - returns the total seconds
*)

PROCEDURE totalSeconds (second, minute, hour,
                        day, month, year: LONGCARD) : LONGCARD ;
VAR
   result: LONGCARD ;
BEGIN
   result := second
             + minute * (MAX (Sec) + 1)
             + hour * ((MAX (Min) + 1) * (MAX (Sec) + 1))
             + ((daysInYear (day, month, year) + totalYear (year))
                * ((MAX (Hour) + 1) * ((MAX (Min) + 1) * (MAX (Sec) + 1)))) ;
   RETURN result
END totalSeconds ;


PROCEDURE SetClock (userData: DateTime);
VAR
   ts       : timespec ;
   nano, sec: LONGCARD ;
   offset   : LONGINT ;
BEGIN
   IF Debugging
   THEN
      sec := totalSeconds (userData.second, userData.minute, userData.hour,
                           VAL (CARDINAL, userData.day) - MIN (Day),
                           userData.month, userData.year) ;
      printf ("setclock = %ld\n", sec);
      offset := timezone () ;
      sec := VAL (LONGINT, sec) - offset ;
      printf ("setclock = %ld\n", sec);
   END ;
   IF CanSetClock ()
   THEN
      ts := InitTimespec () ;
      nano := VAL (LONGCARD, userData.fractions * 1000) ;
      sec := totalSeconds (userData.second, userData.minute, userData.hour,
                           VAL (CARDINAL, userData.day) - MIN (Day),
                           userData.month, userData.year) ;
      offset := timezone () ;
      sec := VAL (LONGINT, sec) - offset ;
      IF SetTimespec (ts, sec, nano) = 1
      THEN
         IF SetTimeRealtime (ts) = 0
         THEN
         END
      END ;
      ts := KillTimespec (ts)
   END
END SetClock ;


BEGIN
   known := FALSE ;
   canset := FALSE ;
   canget := FALSE
END SysClock.
