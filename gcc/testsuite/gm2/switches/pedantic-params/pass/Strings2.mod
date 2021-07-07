(* Copyright (C) 2005, 2006 Free Software Foundation, Inc. *)
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
(* ---------------------------------------------------------------------
 * This program is copyright (c) 1996 Faculty of Information Technology,
 *  Queensland University of Technology, Brisbane, Australia.
 *  The program may be freely distributed in source or compiled form,
 *  provided this copyright notice remains intact in the sources.
 * --------------------------------------------------------------------- *)

(****************************************************************
$Log: Strings2.mod,v $
Revision 1.3  2006/01/11 00:12:52  gaius
added 2006 to all Copyright dates

Revision 1.2  2005/11/18 21:54:50  gaius
fixed Copyright dates and added GPL notices to all files.

Revision 1.1  2004/10/26 14:40:19  gaius
added switch pass and fail tests

Revision 1.2  2004/10/17 11:46:06  iztok
*** empty log message ***

Revision 1.1.1.1  2004/10/06 05:46:24  iztok
Gardens Point Modula-2 ISO libs directory

Revision 1.1  2003/11/04 21:34:13  iztokk
added library sources and some document stuff

Revision 1.1  1996/09/06 07:51:32  lederman
Initial revision

*)

IMPLEMENTATION MODULE Strings2;
(*
 * Purpose:
 *   Facilities for manipulating strings
 *
 * Log:
 *   25/08/96  JL  Initial Release
 *
 * Acknowledgments:
 *   All procedures are substantially based on the existing
 *   GPM module StdStrings
 *
 * Notes:
 *   Complies with ISO/IEC 10514-1:1996
 *
 *)

IMPORT ASCII;


PROCEDURE Length (stringVal : ARRAY OF CHAR) : CARDINAL;
BEGIN
  RETURN LENGTH(stringVal);
END Length;


PROCEDURE CanAssignAll (sourceLength: CARDINAL; 
			VAR destination: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN sourceLength <= HIGH(destination) + 1;
END CanAssignAll;


PROCEDURE Assign (source: ARRAY OF CHAR; VAR destination: ARRAY OF CHAR);
  VAR ix, smaller : CARDINAL;
      ch : CHAR;
BEGIN
  IF HIGH(source) < HIGH(destination) THEN
    smaller := HIGH(source);
  ELSE
    smaller := HIGH(destination);
  END;
  FOR ix := 0 TO smaller DO
    ch := source[ix];
    destination[ix] := ch;
    IF ch = ASCII.nul THEN RETURN END;
  END;
  IF smaller < HIGH(destination) THEN
    destination[smaller + 1] := ASCII.nul;
  END;
END Assign;


PROCEDURE CanExtractAll (sourceLength, startIndex, numberToExtract: CARDINAL;
                         VAR destination: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN (startIndex + numberToExtract <= sourceLength) AND
	 (numberToExtract <= HIGH(destination) + 1);
END CanExtractAll;


PROCEDURE Extract (source: ARRAY OF CHAR; startIndex, numberToExtract: CARDINAL;
                   VAR destination: ARRAY OF CHAR);
  VAR dIx, lim : CARDINAL;
BEGIN
  dIx := 0;
  IF numberToExtract > 0 THEN (* else special case *)
    DEC(numberToExtract);
    IF numberToExtract > HIGH(destination) THEN
      numberToExtract := HIGH(destination);
    END;
    lim := LENGTH(source);
    IF startIndex + numberToExtract >= lim THEN DEC(lim);
    ELSE lim := startIndex + numberToExtract;
    END;
    (* lim is last index to copy *)
    WHILE startIndex <= lim DO
      destination[dIx] := source[startIndex]; INC(startIndex); INC(dIx);
    END;
  END;
  IF dIx <= HIGH(destination) THEN destination[dIx] := ASCII.nul END;
END Extract;


PROCEDURE CanDeleteAll (stringLength, startIndex, numberToDelete: CARDINAL): BOOLEAN;
BEGIN
  RETURN startIndex + numberToDelete <= stringLength;
END CanDeleteAll;


PROCEDURE Delete (VAR string: ARRAY OF CHAR; startIndex, numberToDelete: CARDINAL);
  VAR lim, mIx : CARDINAL;
BEGIN
  lim := LENGTH(string);
  IF startIndex < lim THEN (* else do nothing *)
    IF startIndex + numberToDelete <= lim THEN (* else startIndex is unchanged *)
      mIx := startIndex + numberToDelete;
      WHILE mIx < lim DO
        string[startIndex] := string[mIx]; INC(startIndex); INC(mIx);
      END;
    END;
    IF startIndex <= HIGH(string) THEN string[startIndex] := ASCII.nul END;
  END;
END Delete;


PROCEDURE CanInsertAll (sourceLength, startIndex: CARDINAL;
                        VAR destination: ARRAY OF CHAR): BOOLEAN;
  VAR dLen : CARDINAL;
BEGIN
  dLen := LENGTH(destination);
  RETURN (startIndex <= dLen) AND (sourceLength + dLen <= HIGH(destination) +1);
END CanInsertAll;


PROCEDURE Insert (source: ARRAY OF CHAR; startIndex: CARDINAL;
                  VAR destination: ARRAY OF CHAR);
  VAR dLen, sLen, rIx, ix : CARDINAL; 
BEGIN
  sLen := LENGTH(source);
  dLen := LENGTH(destination);
  IF (sLen = 0) OR (dLen <= startIndex) THEN RETURN END; (* trivial cases *)

  (* copy excess characters up *)
  rIx := dLen + sLen;
  IF rIx > HIGH(destination) THEN
    rIx := HIGH(destination); 
    IF rIx >= (sLen + startIndex) THEN
      FOR ix := rIx - sLen TO startIndex BY -1 DO
        destination[rIx] := destination[ix]; DEC(rIx);
      END;
    END;
  ELSE 
    destination[rIx] := ASCII.nul; DEC(rIx);
    FOR ix := dLen - 1 TO startIndex BY -1 DO
      destination[rIx] := destination[ix]; DEC(rIx);
    END;
  END;

  (* now copy in source string *)
  DEC(sLen); (* assert : was not zero previously *)
  IF sLen + startIndex > HIGH(destination) THEN
    sLen := HIGH(destination) - startIndex;
  END;
  FOR ix := 0 TO sLen DO
    destination[startIndex] := source[ix]; INC(startIndex);
  END;
END Insert;


PROCEDURE CanReplaceAll (sourceLength, startIndex: CARDINAL;
                         VAR destination: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN sourceLength + startIndex <= LENGTH(destination);
END CanReplaceAll;


PROCEDURE Replace (source: ARRAY OF CHAR; startIndex: CARDINAL;
                   VAR destination: ARRAY OF CHAR);
  VAR dLen, ix : CARDINAL;
BEGIN
  dLen := LENGTH(destination);
  ix := 0;
  WHILE (startIndex < dLen)  AND
	(ix <= HIGH(source)) AND (source[ix] <> ASCII.nul) DO
    destination[startIndex] := source[ix]; INC(ix); INC(startIndex);
  END;
END Replace;


PROCEDURE CanAppendAll (sourceLength: CARDINAL; 
		    VAR destination: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN LENGTH(destination) + sourceLength <= HIGH(destination) + 1;
END CanAppendAll;


PROCEDURE Append (source: ARRAY OF CHAR; VAR destination: ARRAY OF CHAR);
  VAR sIx, dIx, lim : CARDINAL;
BEGIN
  dIx := LENGTH(destination);
  IF HIGH(source) + dIx < HIGH(destination) THEN
    lim := HIGH(source) + dIx;
  ELSE
    lim := HIGH(destination);
  END;
  sIx := 0;
  WHILE dIx <= lim DO
    destination[dIx] := source[sIx];
    IF source[sIx] = ASCII.nul THEN RETURN END;
    INC(sIx); INC(dIx);
  END;
  IF dIx <= HIGH(destination) THEN destination[dIx] := ASCII.nul END;
END Append;


PROCEDURE CanConcatAll (source1Length, source2Length: CARDINAL;
                        VAR destination: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN source1Length + source2Length <= HIGH(destination) + 1;
END CanConcatAll;


PROCEDURE Concat (source1, source2: ARRAY OF CHAR; VAR destination: ARRAY OF CHAR);
  VAR dIx, sIx, lim : CARDINAL;
BEGIN
  IF HIGH(source1) < HIGH(destination) THEN
    lim := HIGH(source1);
  ELSE
    lim := HIGH(destination);
  END;
  dIx := 0;
  WHILE (dIx <= lim) AND (source1[dIx] <> ASCII.nul) DO
    destination[dIx] := source1[dIx]; INC(dIx);
  END;
  
  IF HIGH(source2) + lim < HIGH(destination) THEN
    lim := HIGH(source2) + lim;
  ELSE
    lim := HIGH(destination);
  END;
  sIx := 0;
  WHILE dIx <= lim DO
    destination[dIx] := source2[sIx];
    IF source2[sIx] = ASCII.nul THEN RETURN END;
    INC(sIx); INC(dIx);
  END;
  IF dIx <= HIGH(destination) THEN destination[dIx] := ASCII.nul END;
END Concat;


PROCEDURE Capitalize(VAR stringVar : ARRAY OF CHAR);
  VAR ix : CARDINAL;
BEGIN
  ix := 0;
  WHILE (ix <= HIGH(stringVar)) AND (stringVar[ix] <> ASCII.nul) DO
    stringVar[ix] := CAP(stringVar[ix]);
    INC(ix);
  END;
END Capitalize;


(*
 *   There are two different versions of Compare here.
 *   They use different algorithms which have been
 *   tuned for statistically good behaviour. The first
 *   one is usually faster, but neither is as good
 *   as the libc's strncmp.
 *
 * Is it worthwhile to define an interface module to strncmp?
 *
 *)
PROCEDURE Compare      (stringVal1 : ARRAY OF CHAR;
			stringVal2 : ARRAY OF CHAR) : CompareResults;
  VAR s1, s2  : CHAR;
      ix, lim : CARDINAL;
BEGIN
  s1 := stringVal1[0]; s2 := stringVal2[0];
  IF    s1 < s2 THEN RETURN less;
  ELSIF s1 > s2 THEN RETURN greater;
  ELSE
    lim := HIGH(stringVal1);
    IF lim > HIGH(stringVal2) THEN lim := HIGH(stringVal2) END;
    ix := 0;
    IF stringVal1[lim] <> stringVal2[lim] THEN  (* loop terminates before smaller high *)
      LOOP
        IF s1 = s2 THEN
	  IF s2 = ASCII.nul THEN RETURN equal END;
	  INC(ix); s1:= stringVal1[ix]; s2 := stringVal2[ix];
        ELSIF s1 < s2 THEN RETURN less;
        ELSE RETURN greater;
        END;
      END;
    END;
    WHILE ix < lim DO
      IF s1 = s2 THEN
	IF s2 = ASCII.nul THEN RETURN equal END;
	INC(ix); s1:= stringVal1[ix]; s2 := stringVal2[ix];
      ELSIF s1 < s2 THEN RETURN less;
      ELSE RETURN greater;
      END;
    END;
    (* Assert((s1 = s2) AND (ix = lim)); *)
    IF    s2 = ASCII.nul   THEN RETURN equal;
    ELSIF HIGH(stringVal2) > lim THEN
      IF stringVal2[lim + 1] = ASCII.nul THEN RETURN equal ELSE RETURN less END;
    ELSIF HIGH(stringVal1) > lim THEN
      IF stringVal1[lim + 1] = ASCII.nul THEN RETURN equal ELSE RETURN greater END;
    END;
    RETURN equal;
  END;
END Compare;

(* --------------------------------------------------------
PROCEDURE Compare      (str1 : ARRAY OF CHAR;
			str2 : ARRAY OF CHAR) : CompareResults;
  VAR s1, s2 : CHAR; ix, lim, lim1, lim2 : CARDINAL;
BEGIN
  IF str1[0] <> str2[0] THEN 
    IF str1[0] > str2[0] THEN RETURN less;
    ELSE RETURN greater;
    END;
  ELSE 
    IF str1[0] = ASCII.nul THEN RETURN equal END;
    lim1 := HIGH(str1) + 1;
    lim2 := HIGH(str2) + 1;
    s1 := str1[1]; s2 := str2[1]; ix := 1;
    IF str2[lim2] = ASCII.nul THEN (* simple is safe *)
      WHILE s1 = s2 DO
	IF s1 = ASCII.nul THEN RETURN equal END;
	INC(ix); s1 := str1[ix]; s2 := str2[ix];
      END;
      (* chars are different *)
      IF ix >= lim1 THEN
	IF s2 = ASCII.nul THEN RETURN equal;
        ELSE RETURN less;
	END;
      ELSIF s1 < s2 THEN RETURN less;
      ELSE RETURN greater;
      END;
    ELSIF str1[lim1] = ASCII.nul THEN (* simple is safe *)
      WHILE s1 = s2 DO
	IF s1 = ASCII.nul THEN RETURN equal END;
	INC(ix); s1 := str1[ix]; s2 := str2[ix];
      END;
      (* chars are different *)
      IF ix >= lim2 THEN
	IF s1 = ASCII.nul THEN RETURN equal;
        ELSE RETURN greater;
	END;
      ELSIF s1 < s2 THEN RETURN less;
      ELSE RETURN greater;
      END;
    ELSE (* must do full test *)
      IF lim1 <= lim2 THEN lim := lim1 ELSE lim := lim2 END;
      WHILE ix < lim DO
	IF s1 = s2 THEN
	  IF s2 = ASCII.nul THEN RETURN equal;
	  ELSE INC(ix); s1 := str1[ix]; s2 := str2[ix];
	  END;
        ELSIF s1 < s2 THEN RETURN less;
        ELSE RETURN greater;
        END;
      END;
      IF lim2 > lim THEN (* only lim1 is known to be ended *)
        IF s2 <> ASCII.nul THEN RETURN less;
        ELSE RETURN equal;
        END;
      ELSIF lim1 > lim THEN
        IF s1 <> ASCII.nul THEN RETURN greater;
        ELSE RETURN equal;
        END;
      ELSE RETURN equal;
      END;
    END;
  END;
END Compare;
-------------------------------------------------------- *)


PROCEDURE Equal (stringVal1, stringVal2: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN Compare(stringVal1, stringVal2) = equal;
END Equal;


PROCEDURE FindNext (pattern, stringToSearch: ARRAY OF CHAR; startIndex: CARDINAL;
                    VAR patternFound: BOOLEAN; VAR posOfPattern: CARDINAL);
  VAR sLen, pLen, px, sx : CARDINAL;
BEGIN
  sLen := LENGTH(stringToSearch);
  pLen := LENGTH(pattern);
  IF pLen > sLen THEN patternFound := FALSE; RETURN END;

  IF pLen = 0 THEN 		(* What are the semantics here? *)
    patternFound := startIndex < sLen;
    posOfPattern := startIndex;	(* Martin says "" matches any character *)
    RETURN;
  END;

  WHILE startIndex <= sLen - pLen DO	(* find potential starting points *)
    IF stringToSearch[startIndex] = pattern[0] THEN
      px := 0; sx := startIndex;	(* now compare strings *)
      LOOP
	INC(px); INC(sx);
	IF px = pLen THEN
	  patternFound := TRUE;
	  posOfPattern := startIndex;
	  RETURN;
	ELSIF pattern[px] <> stringToSearch[sx] THEN
	  EXIT;
	END;
      END; (* loop *)
    END;
    INC(startIndex);
  END;
  patternFound := FALSE;
END FindNext;


PROCEDURE FindPrev (pattern, stringToSearch: ARRAY OF CHAR; startIndex: CARDINAL;
                    VAR patternFound: BOOLEAN; VAR posOfPattern: CARDINAL);
  VAR sLen, pLen, sx, px : CARDINAL;
BEGIN
  sLen := LENGTH(stringToSearch);
  pLen := LENGTH(pattern);
  IF pLen > sLen THEN patternFound := FALSE; RETURN END;

  IF pLen = 0 THEN 		(* What are the semantics here? *)
    patternFound := TRUE;
    IF startIndex >= sLen THEN
      posOfPattern := sLen - 1;
    ELSE
      posOfPattern := startIndex;
    END;
    RETURN;
  END;

 (* if startIndex > sLen - pLen the whole string is searched *)
  IF startIndex > sLen - pLen THEN startIndex := sLen - pLen END;

  (* now find potential starting points *)
  INC(startIndex);  (* so it doesn't go negative *)
  WHILE startIndex > 0 DO
    DEC(startIndex);
    IF stringToSearch[startIndex] = pattern[0] THEN
      px := 0; sx := startIndex;	(* now compare strings *)
      LOOP
	INC(px); INC(sx);
	IF px = pLen THEN
	  patternFound := TRUE;
	  posOfPattern := startIndex;
	  RETURN;
	ELSIF pattern[px] <> stringToSearch[sx] THEN
	  EXIT;
	END;
      END; (* loop *)
    END;
  END;
  patternFound := FALSE;
END FindPrev;


PROCEDURE FindDiff (stringVal1, stringVal2: ARRAY OF CHAR;
                    VAR differenceFound: BOOLEAN; VAR posOfDifference: CARDINAL);
  VAR ix : CARDINAL;
BEGIN
  ix := 0;
  IF stringVal1[0] = stringVal2[0] THEN
    IF stringVal1[0] = ASCII.nul THEN (* both strings empty *)
      differenceFound := FALSE; RETURN;
    END;
    LOOP
      INC(ix);
      IF (ix > HIGH(stringVal1)) OR (stringVal1[ix] = ASCII.nul) THEN  (* 1 ended *)
        IF (ix > HIGH(stringVal2)) OR (stringVal2[ix] = ASCII.nul) THEN (* 2 also *)
	  differenceFound := FALSE; RETURN;	   (* both ended, and equal *)
	ELSE
	  EXIT;	(* only 1 ended *)
	END;
      ELSIF (ix > HIGH(stringVal2)) OR (stringVal2[ix] = ASCII.nul) THEN
	EXIT;	(* only 2 ended *)
      ELSIF stringVal1[ix] <> stringVal2[ix] THEN
	EXIT;	(* strings differ *)
      (* else go around the loop once more *)
      END;
    END; (* loop *)
  END;
  differenceFound := TRUE;
  posOfDifference := ix;
END FindDiff;

END Strings2.
