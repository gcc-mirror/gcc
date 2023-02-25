(* DynamicPath.mod implements a path for DynamicStrings.

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

IMPLEMENTATION MODULE DynamicPath ;  (*!m2iso+gm2*)

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM DynamicStrings IMPORT InitString, ConCat, ConCatChar, char, Dup,
                           KillString, Length, EqualArray ;
FROM SFIO IMPORT Exists ;
FROM FIO IMPORT StdErr ;
FROM M2Printf IMPORT fprintf0, fprintf1 ;

CONST
   Directory = '/' ;
   Debugging = FALSE ;

TYPE
   PathList = POINTER TO RECORD
                            tail,
                            next : PathList ;
                            entry: String ;
                         END ;


VAR
   FreeList: PathList ;


(*
   KillPathList - places list pl onto the freelist.
                  Postcondition: pl will be NIL.
*)

PROCEDURE KillPathList (VAR pl: PathList) ;
BEGIN
   IF pl # NIL
   THEN
      pl^.tail^.next := FreeList ;
      FreeList := pl ;
      pl := NIL
   END
END KillPathList ;


(*
   InitPathList - creates a new empty path list.
                  It takes a copy of the string.
*)

PROCEDURE InitPathList (str: String) : PathList ;
VAR
   pl: PathList ;
BEGIN
   NEW (pl) ;
   WITH pl^ DO
      tail := pl ;
      next := NIL ;
      entry := Dup (str)
   END ;
   RETURN pl
END InitPathList ;


(*
   Cons - appends str to the end of a path list.
          If pl is NIL a new list is created and returned
          containing str.
*)

PROCEDURE Cons (pl: PathList; str: String) : PathList ;
BEGIN
   IF pl = NIL
   THEN
      pl := InitPathList (str)
   ELSE
      pl := ConsList (pl, InitPathList (str))
   END ;
   RETURN pl
END Cons ;


(*
   ConsList - concatenates path list left and right together.
*)

PROCEDURE ConsList (left, right: PathList) : PathList ;
BEGIN
   IF right # NIL
   THEN
      left^.tail^.next := right ;
      left^.tail := right^.tail
   END ;
   RETURN left
END ConsList ;


(*
   Stash - returns pl before setting pl to NIL.
*)

PROCEDURE Stash (VAR pl: PathList) : PathList ;
VAR
   old: PathList ;
BEGIN
   old := pl ;
   pl := NIL ;
   RETURN old
END Stash ;


(*
   AddDir - if str is not empty and does not end with / then add
            a directory.
            Postcondition: str is returned (with a '/' at the end)
            or is empty.
*)

PROCEDURE AddDir (str: String) : String ;
BEGIN
   IF Length (str) > 0
   THEN
      IF char (str, -1) # Directory
      THEN
         str := ConCatChar (str, Directory)
      END
   END ;
   RETURN str
END AddDir ;


(*
   FindFileName - returns NIL if a file cannot be found otherwise
                  it returns the path including the filename.
*)

PROCEDURE FindFileName (filename: String; pl: PathList) : String ;
VAR
   s: String ;
BEGIN
   WHILE pl # NIL DO
      s := ConCat (AddDir (Dup (pl^.entry)), Dup (filename)) ;
      IF Debugging
      THEN
         fprintf1 (StdErr, "testing for %s: ", s)
      END ;
      IF Exists (s)
      THEN
         IF Debugging
         THEN
            fprintf0 (StdErr, "yes\n")
         END ;
         RETURN s
      END ;
      IF Debugging
      THEN
         fprintf0 (StdErr, "no\n")
      END ;
      s := KillString (s) ;
      pl := pl^.next
   END ;
   IF Debugging
   THEN
      fprintf1 (StdErr, "FindFileName did not find: %s in path\n", filename)
   END ;
   RETURN NIL
END FindFileName ;


(*
   DumpPath - debugging dump of the pathlist.
*)

PROCEDURE DumpPath (name: String; pl: PathList) ;
BEGIN
   fprintf1 (StdErr, "%s:", name) ;
   WHILE pl # NIL DO
      fprintf0 (StdErr, " {") ;
      fprintf1 (StdErr, "%s", pl^.entry) ;
      fprintf0 (StdErr, "}") ;
      pl := pl^.next
   END ;
   fprintf0 (StdErr, "\n")
END DumpPath ;


BEGIN
   FreeList := NIL
END DynamicPath.
