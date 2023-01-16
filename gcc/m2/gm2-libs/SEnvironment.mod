(* SEnvironment.mod provides access to the environment of a process.

Copyright (C) 2001-2023 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE SEnvironment ;

FROM DynamicStrings IMPORT string, InitStringCharStar,
                           InitStringDB, InitStringCharStarDB,
                           InitStringCharDB, MultDB, DupDB, SliceDB ;

FROM libc IMPORT getenv, putenv ;

(*
#undef GM2_DEBUG_SENVIRONMENT
if defined(GM2_DEBUG_SENVIRONMENT)
#  define InitString(X) InitStringDB(X, __FILE__, __LINE__)
#  define InitStringCharStar(X) InitStringCharStarDB(X, __FILE__, __LINE__)
#  define InitStringChar(X) InitStringCharDB(X, __FILE__, __LINE__)
#  define Mult(X,Y) MultDB(X, Y, __FILE__, __LINE__)
#  define Dup(X) DupDB(X, __FILE__, __LINE__)
#  define Slice(X,Y,Z) SliceDB(X, Y, Z, __FILE__, __LINE__)
#endif
*)


(*
   GetEnvironment - gets the environment variable Env and places
      	       	    a copy of its value into String, dest.
                    It returns TRUE if the string Env was found in
                    the processes environment.
*)

PROCEDURE GetEnvironment (Env: String;
                          VAR dest: String) : BOOLEAN ;
VAR
   Addr: POINTER TO CHAR ;
BEGIN
   IF Env=NIL
   THEN
      dest := NIL ;
      RETURN FALSE
   ELSE
      Addr := getenv (string (Env)) ;
      IF Addr=NIL
      THEN
         dest := NIL ;
         RETURN FALSE
      ELSE
         dest := InitStringCharStar (Addr) ;
         RETURN TRUE
      END
   END
END GetEnvironment ;


(*
   PutEnvironment - change or add an environment variable definition EnvDef.
                    TRUE is returned if the environment variable was
                    set or changed successfully.
*)

PROCEDURE PutEnvironment (EnvDef: String) : BOOLEAN ;
BEGIN
   RETURN putenv (string (EnvDef)) = 0
END PutEnvironment ;


END SEnvironment.
