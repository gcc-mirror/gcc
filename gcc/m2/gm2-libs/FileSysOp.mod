(* FileSysOp.mod provides procedures to manipulate the file system.

Copyright (C) 2025 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaiusmod2@gmail.com>.

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

IMPLEMENTATION MODULE FileSysOp ;

IMPORT StringFileSysOp ;
FROM DynamicStrings IMPORT String, InitString, KillString ;


(*
   Description: provides access to filesystem operations using
                Modula-2 base types.
*)

PROCEDURE Exists (filename: ARRAY OF CHAR) : BOOLEAN ;
VAR
   fn    : String ;
   result: BOOLEAN ;
BEGIN
   fn := InitString (filename) ;
   result := StringFileSysOp.Exists (fn) ;
   fn := KillString (fn) ;
   RETURN result
END Exists ;


PROCEDURE IsDir (dirname: ARRAY OF CHAR) : BOOLEAN ;
VAR
   fn    : String ;
   result: BOOLEAN ;
BEGIN
   fn := InitString (dirname) ;
   result := StringFileSysOp.IsDir (fn) ;
   fn := KillString (fn) ;
   RETURN result
END IsDir ;


PROCEDURE IsFile (filename: ARRAY OF CHAR) : BOOLEAN ;
VAR
   fn    : String ;
   result: BOOLEAN ;
BEGIN
   fn := InitString (filename) ;
   result := StringFileSysOp.IsFile (fn) ;
   fn := KillString (fn) ;
   RETURN result
END IsFile ;


PROCEDURE Unlink (filename: ARRAY OF CHAR) : BOOLEAN ;
VAR
   fn    : String ;
   result: BOOLEAN ;
BEGIN
   fn := InitString (filename) ;
   result := StringFileSysOp.Unlink (fn) ;
   fn := KillString (fn) ;
   RETURN result
END Unlink ;


PROCEDURE Access (pathname: ARRAY OF CHAR; mode: AccessMode) : AccessMode ;
VAR
   pn    : String ;
   result: AccessMode ;
BEGIN
   pn := InitString (pathname) ;
   result := StringFileSysOp.Access (pn, mode) ;
   pn := KillString (pn) ;
   RETURN result
END Access ;


END FileSysOp.
