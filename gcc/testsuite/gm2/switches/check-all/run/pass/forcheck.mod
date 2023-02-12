(* Copyright (C) 2009 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

MODULE forcheck ;

FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;


PROCEDURE foo (init: CARDINAL);
VAR
   j: CARDINAL;
(*
   T21 : (* <!g> *) INTEGER ;
   T24 : (* <!g> *) INTEGER ;
   T25 : (* <!g> *) INTEGER ;
   T26 : (* <!g> *) INTEGER ;
   T27 : (* <!g> *) LONGINT ;
 *)
BEGIN
(*
   T21 := init - 1 ;
   j := T21 ;
   T24 := VAL ((* <!g> *) INTEGER, 1 - T21) ;
   T25 := -T24 ;
   T26 := -T25 ;
   T27 := VAL ((* <!g> *) LONGINT, T21 + T26) ;
   IF T27 < 0
   THEN
      WriteString("ForLoopToException\n") ; WriteLn
   END ;
   IF T27 > 4294967295
   THEN
      WriteString("ForLoopToException\n") ; WriteLn
   END ;
*)
   FOR j:= init-1 TO 1 BY -1 DO
      WriteString('value of j ') ; WriteCard(j, 0) ; WriteLn
   END
END foo ;


BEGIN
   foo(3)
END forcheck.
