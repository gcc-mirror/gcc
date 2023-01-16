(* mcComment.mod provides a module to remember the comments.

Copyright (C) 2015-2023 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE mcComment ;   (*!m2pim*)

FROM DynamicStrings IMPORT String, InitString, ConCat, RemoveWhitePrefix, Mark, KillString, InitStringCharStar, EqualCharStar, Length, Slice, string, char ;
FROM Storage IMPORT ALLOCATE ;
FROM nameKey IMPORT Name, keyToCharStar, lengthKey, NulName ;
FROM mcDebug IMPORT assert ;
FROM ASCII IMPORT nl ;
FROM libc IMPORT printf ;


TYPE
   commentType = (unknown, procedureHeading, inBody, afterStatement) ;

   commentDesc = POINTER TO RECORD
                               type    :  commentType ;
			       content :  String ;
			       procName:  Name ;
			       used    :  BOOLEAN ;
                            END ;



(*
   isProcedureComment - returns TRUE if, cd, is a procedure comment.
*)

PROCEDURE isProcedureComment (cd: commentDesc) : BOOLEAN ;
BEGIN
   RETURN (cd # NIL) AND (cd^.type = procedureHeading)
END isProcedureComment;


(*
   isBodyComment - returns TRUE if, cd, is a body comment.
*)

PROCEDURE isBodyComment (cd: commentDesc) : BOOLEAN ;
BEGIN
   RETURN (cd # NIL) AND (cd^.type = inBody)
END isBodyComment;


(*
   isAfterComment - returns TRUE if, cd, is an after comment.
*)

PROCEDURE isAfterComment (cd: commentDesc) : BOOLEAN ;
BEGIN
   RETURN (cd # NIL) AND (cd^.type = afterStatement)
END isAfterComment;


(*
   initComment - the start of a new comment has been seen by the lexical analyser.
                 A new comment block is created and all addText contents are placed
                 in this block.  onlySpaces indicates whether we have only seen
                 spaces on this line.
*)

PROCEDURE initComment (onlySpaces: BOOLEAN) : commentDesc ;
VAR
   cd: commentDesc ;
BEGIN
   NEW (cd) ;
   assert (cd # NIL) ;
   WITH cd^ DO
      IF onlySpaces
      THEN
         type := inBody
      ELSE
         type := afterStatement
      END ;
      content := InitString ('') ;
      procName := NulName ;
      used := FALSE
   END ;
   RETURN cd
END initComment ;


(*
   addText - cs is a C string (null terminated) which contains comment text.
             This is appended to the comment, cd.
*)

PROCEDURE addText (cd: commentDesc; cs: ADDRESS) ;
BEGIN
   IF cd # NIL
   THEN
      cd^.content := ConCat (cd^.content, InitStringCharStar (cs))
   END
END addText ;


(*
   Min - returns the lower of, a, and, b.
*)

PROCEDURE Min (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a < b
   THEN
      RETURN a
   ELSE
      RETURN b
   END
END Min ;


(*
   RemoveNewlines -
*)

PROCEDURE RemoveNewlines (s: String) : String ;
BEGIN
   WHILE Length (s) > 0 DO
      IF char (s, 0) = nl
      THEN
         s := RemoveWhitePrefix (Slice (s, 1, 0))
      ELSE
         RETURN RemoveWhitePrefix (s)
      END
   END ;
   RETURN s
END RemoveNewlines ;


(*
   seenProcedure - returns TRUE if the name, procName, appears as the first word
                   in the comment.
*)

PROCEDURE seenProcedure (cd: commentDesc; procName: Name) : BOOLEAN ;
VAR
   s   : String ;
   a   : ADDRESS ;
   i, h: CARDINAL ;
   res : BOOLEAN ;
BEGIN
   a := keyToCharStar (procName) ;
   s := RemoveNewlines (cd^.content) ;
   s := Slice (Mark (s), 0, Min (Length (s), lengthKey (procName))) ;
   res := EqualCharStar (s, a) ;
   s := KillString (s) ;
   RETURN res
END seenProcedure ;


(*
   setProcedureComment - changes the type of comment, cd, to a
                         procedure heading comment,
                         providing it has the procname as the first word.
*)

PROCEDURE setProcedureComment (cd: commentDesc; procname: Name) ;
BEGIN
   IF cd # NIL
   THEN
      IF seenProcedure (cd, procname)
      THEN
         cd^.type := procedureHeading ;
         cd^.procName := procname
      END
   END
END setProcedureComment ;


(*
   getContent - returns the content of comment, cd.
*)

PROCEDURE getContent (cd: commentDesc) : String ;
BEGIN
   IF cd # NIL
   THEN
      RETURN cd^.content
   END ;
   RETURN NIL
END getContent ;


(*
   getCommentCharStar - returns the C string content of comment, cd.
*)

PROCEDURE getCommentCharStar (cd: commentDesc) : ADDRESS ;
VAR
   s: String ;
BEGIN
   s := getContent (cd) ;
   IF s = NIL
   THEN
      RETURN NIL
   ELSE
      RETURN string (s)
   END
END getCommentCharStar ;


(*
   getProcedureComment - returns the current procedure comment if available.
*)

PROCEDURE getProcedureComment (cd: commentDesc) : String ;
BEGIN
   IF (cd^.type = procedureHeading) AND (NOT cd^.used)
   THEN
      cd^.used := TRUE ;
      RETURN cd^.content
   END ;
   RETURN NIL
END getProcedureComment ;


(*
   getAfterStatementComment - returns the current statement after comment if available.
*)

PROCEDURE getAfterStatementComment (cd: commentDesc) : String ;
BEGIN
   IF (cd^.type = afterStatement) AND (NOT cd^.used)
   THEN
      cd^.used := TRUE ;
      RETURN cd^.content
   END ;
   RETURN NIL
END getAfterStatementComment ;


(*
   getInbodyStatementComment - returns the current statement after comment if available.
*)

PROCEDURE getInbodyStatementComment (cd: commentDesc) : String ;
BEGIN
   IF (cd^.type = inBody) AND (NOT cd^.used)
   THEN
      cd^.used := TRUE ;
      RETURN cd^.content
   END ;
   RETURN NIL
END getInbodyStatementComment ;


(*
   dumpComment -
*)

PROCEDURE dumpComment (cd: commentDesc) ;
BEGIN
   printf ("comment : ");
   WITH cd^ DO
      CASE type OF

      unknown         :  printf ("unknown") |
      procedureHeading:  printf ("procedureheading") |
      inBody          :  printf ("inbody") |
      afterStatement  :  printf ("afterstatement")

      END ;
      IF used
      THEN
         printf (" used")
      ELSE
         printf (" unused")
      END ;
      printf (" contents = %s\n", string (content))
   END
END dumpComment ;


END mcComment.
