(* Keyboard.mod provides compatibility with Logitech 3.0 Keyboard module.

Copyright (C) 2004-2023 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE Keyboard ;

FROM Selective IMPORT SetOfFd, InitSet, KillSet, MaxFdsPlusOne, ReadCharRaw,
                      Timeval, InitTime, KillTime, FdIsSet, FdZero, FdSet,
                      Select ;


CONST
   stdin = 0 ;


(*
   Read - reads a character from StdIn. If necessary it will wait
          for a key to become present on StdIn.
*)

PROCEDURE Read (VAR ch: CHAR) ;
BEGIN
   ch := ReadCharRaw(stdin)
END Read ;


(*
   KeyPressed - returns TRUE if a character can be read from StdIn
                without blocking the caller.
*)

PROCEDURE KeyPressed () : BOOLEAN ;
VAR
   s      : SetOfFd ;
   t      : Timeval ;
   r      : INTEGER ;
   Pressed: BOOLEAN ;
BEGIN
   t := InitTime(0, 0) ;
   s := InitSet() ;
   FdZero(s) ;
   FdSet(stdin, s) ;
   r := Select(MaxFdsPlusOne(stdin, stdin),
               s, NIL, NIL, t) ;
   Pressed := FdIsSet(stdin, s) ;
   s := KillSet(s) ;
   t := KillTime(t) ;
   RETURN( Pressed )
END KeyPressed ;


END Keyboard.
