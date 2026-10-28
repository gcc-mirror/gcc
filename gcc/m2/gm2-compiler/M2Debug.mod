(* M2Debug.mod simple debugging facilities in the Modula-2 compiler.

Copyright (C) 2001-2026 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE M2Debug ;


FROM StrIO IMPORT WriteString, WriteLn ;
FROM M2Error IMPORT InternalError ;
FROM M2Options IMPORT GetInternalDebuggingFlag ;
FROM M2LexBuf IMPORT BuiltinTokenNo ;


(*
   Assert - tests the boolean condition. If false then an error is reported
            and the execution is HALTed.
*)

PROCEDURE Assert (condition: BOOLEAN) ;
BEGIN
   IF NOT condition
   THEN
      InternalError ('assert failed')
   END
END Assert ;


(*
   WriteDebug - only write string msg if the debugging mode is on.
*)

PROCEDURE WriteDebug (msg: ARRAY OF CHAR) ;
BEGIN
   IF GetInternalDebuggingFlag (BuiltinTokenNo)
   THEN
      WriteString (msg) ; WriteLn
   END
END WriteDebug ;


END M2Debug.
