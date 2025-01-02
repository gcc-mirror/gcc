(* RandomNumber.mod implement a set of random number procedures for pervasive types.

Copyright (C) 2012-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE RandomNumber ;


FROM libc IMPORT rand, srand ;
FROM Selective IMPORT Timeval, InitTime, KillTime, GetTime, GetTimeOfDay ;


(*
   Randomize - initialize the random number generator with a seed
               based on the microseconds.
*)

PROCEDURE Randomize ;
VAR
   t        : Timeval ;
   sec, usec: CARDINAL ;
BEGIN
   t := InitTime(0, 0) ;
   IF GetTimeOfDay(t)=0
   THEN
   END ;
   GetTime(t, sec, usec) ;
   RandomInit(usec) ;
   t := KillTime(t)
END Randomize ;


(*
   RandomInit - initialize the random number generator with value, seed.
*)

PROCEDURE RandomInit (seed: CARDINAL) ;
BEGIN
   srand(seed)
END RandomInit ;


(*
   RandomBytes - fills in an array with random values.
*)

PROCEDURE RandomBytes (VAR a: ARRAY OF BYTE) ;
VAR
   i, h: CARDINAL ;
BEGIN
   h := HIGH(a) ;
   i := 0 ;
   WHILE i<=h DO
      a[i] := VAL(BYTE, rand()) ;
      INC(i)
   END
END RandomBytes ;


(*
   RandomInt - return an INTEGER in the range [low .. high].
*)

PROCEDURE RandomInt (low, high: INTEGER) : INTEGER ;
BEGIN
   RETURN VAL(INTEGER, RandomLongInt(low, high))
END RandomInt ;


(*
   RandomShortInt - return an SHORTINT in the range [low..high].
*)

PROCEDURE RandomShortInt (low, high: SHORTINT) : SHORTINT ;
BEGIN
   RETURN VAL(SHORTINT, RandomInt(low, high))
END RandomShortInt ;


(*
   RandomLongInt - return an LONGINT in the range [low..high].
*)

PROCEDURE RandomLongInt (low, high: LONGINT) : LONGINT ;
VAR
   random: LONGINT ;
   values,
   number: LONGCARD ;
BEGIN
   RandomBytes(number) ;
   IF (low=0) AND (high=0)
   THEN
      RETURN number
   ELSE
      values := high-low;
      random := number MOD (values+1) ;
      RETURN random+low
   END
END RandomLongInt ;


(*
   RandomLongCard - return an LONGCARD in the range [low..high].
*)

PROCEDURE RandomLongCard (low, high: LONGCARD) : LONGCARD ;
VAR
   random,
   values,
   number: LONGCARD ;
BEGIN
   RandomBytes(number) ;
   IF (low=0) AND (high=0)
   THEN
      RETURN number
   ELSE
      values := high-low;
      random := number MOD (values+1) ;
      RETURN random+low
   END
END RandomLongCard ;


(*
   RandomCard - return a CARDINAL in the range [low..high].
*)

PROCEDURE RandomCard (low, high: CARDINAL) : CARDINAL ;
BEGIN
   RETURN RandomLongCard(low, high)
END RandomCard ;


(*
   RandomShortCard - return a SHORTCARD in the range [low..high].
*)

PROCEDURE RandomShortCard (low, high: CARDINAL) : CARDINAL ;
BEGIN
   RETURN RandomLongCard(low, high)
END RandomShortCard ;


(*
   RandomReal - return a REAL number in the range 0.0..1.0
*)

PROCEDURE RandomReal () : REAL ;
BEGIN
   RETURN RandomLongReal()
END RandomReal ;


(*
   RandomLongReal - return a LONGREAL number in the range 0.0..1.0
*)

PROCEDURE RandomLongReal () : LONGREAL ;
VAR
   l: LONGCARD ;
BEGIN
   RandomBytes(l) ;
   RETURN VAL(LONGREAL, l)/VAL(LONGREAL, MAX(LONGCARD))
END RandomLongReal ;


(*
   RandomShortReal - return a SHORTREAL number in the range 0.0..1.0
*)

PROCEDURE RandomShortReal () : SHORTREAL ;
BEGIN
   RETURN RandomLongReal()
END RandomShortReal ;


BEGIN
   Randomize
END RandomNumber.
