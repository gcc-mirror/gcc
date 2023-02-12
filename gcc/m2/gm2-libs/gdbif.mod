(* gdbif.mod enable interactive connectivity with gdb.

Copyright (C) 2011-2023 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE gdbif ;


FROM libc IMPORT printf, getpid, sleep ;
FROM FIO IMPORT File, WriteString, WriteLine, OpenToWrite, Close, IsNoError ;
FROM StringConvert IMPORT itos ;
FROM DynamicStrings IMPORT String, KillString ;
IMPORT SFIO ;

VAR
   invoked,
   mustWait: BOOLEAN ;


(*
   connectSpin - breakpoint placeholder.
*)

PROCEDURE connectSpin ;
BEGIN
   (* do nothing, its purpose is to allow gdb to set breakpoints here.  *)
END connectSpin ;


(*
   sleepSpin - waits for the boolean variable mustWait to become FALSE.
               It sleeps for a second between each test of the variable.
*)

PROCEDURE sleepSpin ;
BEGIN
   IF mustWait
   THEN
      printf ("process %d is waiting for you to:\n", getpid ());
      printf ("(gdb) attach %d\n", getpid ());
      printf ("(gdb) break connectSpin\n");
      printf ("(gdb) print finishSpin()\n");
      REPEAT
         sleep (1);
         printf (".")
      UNTIL NOT mustWait ;
      printf ("ok continuing\n");
      connectSpin
   END
END sleepSpin ;


(*
   finishSpin - sets boolean mustWait to FALSE.
*)

PROCEDURE finishSpin ;
BEGIN
   mustWait := FALSE
END finishSpin ;


(*
   gdbinit -
*)

PROCEDURE gdbinit ;
VAR
   file: File ;
   s   : String ;
BEGIN
   file := OpenToWrite (".gdbinit") ;
   IF IsNoError (file)
   THEN
      WriteString (file, "attach ") ;
      s := SFIO.WriteS (file, itos (getpid (), 6, " ", FALSE)) ;
      WriteString (file, "break connectSpin") ; WriteLine (file) ;
      WriteString (file, "print finishSpin()") ; WriteLine (file) ;
      s := KillString (s) ;
      Close (file) ;
      sleepSpin
   END
END gdbinit ;


BEGIN
   mustWait := TRUE
END gdbif.
