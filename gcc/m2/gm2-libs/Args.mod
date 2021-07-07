(* Args.mod provide access to command line arguments.

Copyright (C) 2001-2021 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

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

IMPLEMENTATION MODULE Args ;


FROM UnixArgs IMPORT ArgC, ArgV ;
FROM ASCII IMPORT nul ;


CONST
   MaxArgs   =  255 ;
   MaxString = 4096 ;


(*
   Source allows us to examine the ArgV contents.
*)

VAR
   Source: POINTER TO ARRAY [0..MaxArgs] OF
           POINTER TO ARRAY [0..MaxString] OF CHAR ;


(*
   GetArg - returns the nth argument from the command line.
            The success of the operation is returned.
*)

PROCEDURE GetArg (VAR a: ARRAY OF CHAR; i: CARDINAL) : BOOLEAN ;
VAR
   High,
   j   : CARDINAL ;
BEGIN
   j := 0 ;
   High := HIGH(a) ;
   IF i<ArgC
   THEN
      Source := ArgV ;
      WHILE (Source^[i]^[j]#nul) AND (j<High) DO
         a[j] := Source^[i]^[j] ;
         INC(j)
      END
   END ;
   IF j<=High
   THEN
      a[j] := nul
   END ;
   RETURN( i<ArgC )
END GetArg ;


(*
   Narg - returns the number of arguments available from
          command line.
*)

PROCEDURE Narg () : CARDINAL ;
BEGIN
   RETURN( ArgC )
END Narg ;


END Args.
