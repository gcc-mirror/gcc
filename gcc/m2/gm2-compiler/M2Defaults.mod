(* M2Defaults.mod provides path and argument defaults.

Copyright (C) 2001-2021 Free Software Foundation, Inc.
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

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE M2Defaults;

FROM DynamicStrings IMPORT InitString, KillString ;
FROM SEnvironment IMPORT GetEnvironment ;


(*
   GetSearchPath - sets string, a, to the environment variable
      	           M2PATH.
*)

PROCEDURE GetSearchPath () : String ;
VAR
   s, p: String ;
BEGIN
   s := InitString('M2PATH') ;
   IF GetEnvironment(s, p)
   THEN
   END ;
   s := KillString(s) ;
   RETURN( p )
END GetSearchPath ;


(*
   GetOptions - returns a string, which is a copy of the environment variable
      	        M2OPTIONS
*)

PROCEDURE GetOptions () : String ;
VAR
   s, p: String ;
BEGIN
   s := InitString('M2OPTIONS') ;
   IF GetEnvironment(s, p)
   THEN
   END ;
   s := KillString(s) ;
   RETURN( p )
END GetOptions ;


END M2Defaults.
