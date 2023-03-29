(* Termbase.mod provides GNU Modula-2 with a PIM 234 compatible Termbase.

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

IMPLEMENTATION MODULE Termbase ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM M2RTS IMPORT Halt ;
IMPORT Display, Keyboard ;

TYPE
   ReadMethods = POINTER TO RECORD
                               r   : ReadProcedure ;
                               s   : StatusProcedure ;
                               next: ReadMethods ;
                            END ;

   WriteMethod = POINTER TO RECORD
                               w   : WriteProcedure ;
                               next: WriteMethod ;
                            END ;

VAR
   rStack: ReadMethods ;
   wStack: WriteMethod ;


(*
   AssignRead - assigns a read procedure and status procedure for terminal
                input. Done is set to TRUE if successful. Subsequent
                Read and KeyPressed calls are mapped onto the user supplied
                procedures. The previous read and status procedures are
                uncovered and reused after UnAssignRead is called.
*)

PROCEDURE AssignRead (rp: ReadProcedure; sp: StatusProcedure;
                      VAR Done: BOOLEAN) ;
VAR
   t: ReadMethods ;
BEGIN
   t := rStack ;
   NEW(rStack) ;
   IF rStack=NIL
   THEN
      Done := FALSE
   ELSE
      WITH rStack^ DO
         r := rp ;
         s := sp ;
         next := t
      END ;
      Done := TRUE
   END
END AssignRead ;


(*
   UnAssignRead - undo the last call to AssignRead and set Done to TRUE
                  on success.
*)

PROCEDURE UnAssignRead (VAR Done: BOOLEAN) ;
VAR
   t: ReadMethods ;
BEGIN
   IF rStack=NIL
   THEN
      Done := FALSE
   ELSE
      Done := TRUE
   END ;
   t := rStack ;
   rStack := rStack^.next ;
   DISPOSE(t)
END UnAssignRead ;


(*
   Read - reads a single character using the currently active read
          procedure.
*)

PROCEDURE Read (VAR ch: CHAR) ;
BEGIN
   IF rStack=NIL
   THEN
      Halt ('no active read procedure', __FILE__, __FUNCTION__, __LINE__)
   ELSE
      rStack^.r(ch)
   END
END Read ;


(*
   KeyPressed - returns TRUE if a character is available to be read.
*)

PROCEDURE KeyPressed () : BOOLEAN ;
BEGIN
   IF rStack=NIL
   THEN
      Halt ('no active status procedure', __FILE__, __FUNCTION__, __LINE__)
   ELSE
      RETURN( rStack^.s() )
   END
END KeyPressed ;


(*
   AssignWrite - assigns a write procedure for terminal output.
                 Done is set to TRUE if successful. Subsequent
                 Write calls are mapped onto the user supplied
                 procedure. The previous write procedure is
                 uncovered and reused after UnAssignWrite is called.
*)

PROCEDURE AssignWrite (wp: WriteProcedure; VAR Done: BOOLEAN) ;
VAR
   t: WriteMethod ;
BEGIN
   t := wStack ;
   NEW(wStack) ;
   IF wStack=NIL
   THEN
      Done := FALSE
   ELSE
      WITH wStack^ DO
         w := wp ;
         next := t
      END ;
      Done := TRUE
   END
END AssignWrite ;


(*
   UnAssignWrite - undo the last call to AssignWrite and set Done to TRUE
                   on success.
*)

PROCEDURE UnAssignWrite (VAR Done: BOOLEAN) ;
VAR
   t: WriteMethod ;
BEGIN
   IF wStack=NIL
   THEN
      Done := FALSE
   ELSE
      Done := TRUE
   END ;
   t := wStack ;
   wStack := wStack^.next ;
   DISPOSE(t)
END UnAssignWrite ;


(*
   Write - writes a single character using the currently active write
           procedure.
*)

PROCEDURE Write (VAR ch: CHAR) ;
BEGIN
   IF wStack=NIL
   THEN
      Halt ('no active write procedure', __FILE__, __FUNCTION__, __LINE__)
   ELSE
      wStack^.w(ch)
   END
END Write ;


(*
   Init -
*)

PROCEDURE Init ;
VAR
   Done: BOOLEAN ;
BEGIN
   rStack := NIL ;
   wStack := NIL ;
   AssignRead(Keyboard.Read, Keyboard.KeyPressed, Done) ;
   IF NOT Done
   THEN
      Halt ('failed to assign read routines from module Keyboard', __FILE__, __FUNCTION__, __LINE__)
   END ;
   AssignWrite(Display.Write, Done) ;
   IF NOT Done
   THEN
      Halt ('failed to assign write routine from module Display', __FILE__, __FUNCTION__, __LINE__)
   END
END Init ;


BEGIN
   Init
END Termbase.
