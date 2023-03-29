(* Strings.mod implement the ISO Strings specification.

Copyright (C) 2008-2023 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE Strings ;

IMPORT ASCII ;
FROM libc IMPORT printf ;

CONST
   Debugging = FALSE ;


(*
   Length - Returns the length of stringVal (the same value as would be returned by the
            pervasive function LENGTH).
*)

PROCEDURE Length (stringVal: ARRAY OF CHAR) : CARDINAL;
BEGIN
   RETURN( LENGTH(stringVal) )
END Length ;


(* The following seven procedures construct a string value, and
   attempt to assign it to a variable parameter.  They all have
   the property that if the length of the constructed string
   value exceeds the capacity of the variable parameter, a
   truncated value is assigned, while if the length of the
   constructed string value is less than the capacity of the
   variable parameter, a string terminator is appended before
   assignment is performed.
*)

(*
   Assign - Copies source to destination.
*)

PROCEDURE Assign (source: ARRAY OF CHAR; VAR destination: ARRAY OF CHAR) ;
VAR
   i,
   sh, dh: CARDINAL ;
BEGIN
   sh := Length(source) ;
   dh := HIGH(destination) ;
   i := 0 ;
   WHILE (i<sh) AND (i<=dh) DO
      destination[i] := source[i] ;
      INC(i)
   END ;
   IF i<=dh
   THEN
      destination[i] := ASCII.nul
   END
END Assign ;


(* Copies at most numberToExtract characters from source to destination,
   starting at position startIndex in source.
*)

PROCEDURE Extract (source: ARRAY OF CHAR;
                   startIndex, numberToExtract: CARDINAL;
                   VAR destination: ARRAY OF CHAR) ;
VAR
   sh, dh,
   i     : CARDINAL ;
BEGIN
   sh := Length(source) ;
   dh := HIGH(destination) ;
   i := 0 ;
   WHILE (i<numberToExtract) AND (startIndex<sh) AND (i<=dh) DO
      destination[i] := source[startIndex] ;
      INC(i) ;
      INC(startIndex)
   END ;
   IF i<=dh
   THEN
      destination[i] := ASCII.nul
   END
END Extract ;


(* Deletes at most numberToDelete characters from stringVar, starting at position
   startIndex.
*)

PROCEDURE Delete (VAR stringVar: ARRAY OF CHAR;
                  startIndex, numberToDelete: CARDINAL) ;
VAR
   h: CARDINAL ;
BEGIN
   IF numberToDelete>0
   THEN
      (* numberToDelete can be consider as the number of characters to skip over *)
      h := Length(stringVar) ;
      WHILE (startIndex+numberToDelete<h) DO
         stringVar[startIndex] := stringVar[startIndex+numberToDelete] ;
         INC(startIndex)
      END ;
      IF startIndex<HIGH(stringVar)
      THEN
         stringVar[startIndex] := ASCII.nul
      END
   END
END Delete ;


(* Inserts source into destination at position startIndex *)

PROCEDURE Insert (source: ARRAY OF CHAR;
                  startIndex: CARDINAL;
                  VAR destination: ARRAY OF CHAR) ;
VAR
   newEnd, endCopy,
   i, j, sh, dh, dl: CARDINAL ;
BEGIN
   sh := Length(source) ;
   dh := HIGH(destination) ;
   dl := Length(destination) ;
   (* make space for source *)
   IF Debugging
   THEN
      printf("sh = %d   dh = %d   dl = %d\n",
             sh, dh, dl);
   END ;
   newEnd := dl+sh ;
   IF newEnd>dh
   THEN
      (* insert will truncate destination *)
      newEnd := dh
   END ;
   IF newEnd>sh
   THEN
      endCopy := newEnd-sh
   ELSE
      endCopy := 0
   END ;
   IF Debugging
   THEN
      printf("\ndestination contains\n%s\nnewEnd = %d   endCopy = %d\n", destination, newEnd, endCopy) ;
      printf("newEnd = %d\n", newEnd) ;
      printf("endCopy = %d\n", endCopy) ;
   END ;
   INC(newEnd) ;
   INC(endCopy) ;
   WHILE endCopy>startIndex DO
      DEC(newEnd) ;
      DEC(endCopy) ;
      IF Debugging
      THEN
         printf("copying dest %d to %d (%c) (startIndex=%d)\n",
                endCopy, newEnd, destination[newEnd], startIndex)
      END ;
      destination[newEnd] := destination[endCopy]
   END ;
   IF Debugging
   THEN
      printf("destination now contains %s\n", destination)
   END ;
   (* copy source into destination *)
   j := startIndex ;
   i := 0 ;
   WHILE (i<sh) AND (j<=dh) DO
      destination[j] := source[i] ;
      INC(i) ;
      INC(j)
   END
END Insert ;


(* Copies source into destination, starting at position startIndex. Copying stops when
   all of source has been copied, or when the last character of the string value in
   destination has been replaced.  *)

PROCEDURE Replace (source: ARRAY OF CHAR;
                   startIndex: CARDINAL;
                   VAR destination: ARRAY OF CHAR) ;
VAR
   i, sh, dh: CARDINAL ;
BEGIN
   i := 0 ;
   sh := Length(source) ;
   dh := Length(destination) ;
   WHILE (i<sh) AND (startIndex<dh) DO
      destination[startIndex] := source[i] ;
      INC(i) ;
      INC(startIndex)
   END ;
   IF startIndex<HIGH(destination)
   THEN
      destination[startIndex] := ASCII.nul
   END
END Replace ;


PROCEDURE Append (source: ARRAY OF CHAR; VAR destination: ARRAY OF CHAR) ;
  (* Appends source to destination. *)
VAR
   i, j, sh, dh: CARDINAL ;
BEGIN
   j := Length(destination) ;
   dh := HIGH(destination) ;
   sh := Length(source) ;
   i := 0 ;
   WHILE (i<sh) AND (j<=dh) DO
      destination[j] := source[i] ;
      INC(i) ;
      INC(j)
   END ;
   IF j<=dh
   THEN
      destination[j] := ASCII.nul
   END
END Append ;


PROCEDURE Concat (source1, source2: ARRAY OF CHAR; VAR destination: ARRAY OF CHAR);
  (* Concatenates source2 onto source1 and copies the result into destination. *)
BEGIN
   Assign(source1, destination) ;
   Append(source2, destination)
END Concat ;


(* The following predicates provide for pre-testing of the operation-completion
   conditions for the procedures above.
*)

PROCEDURE CanAssignAll (sourceLength: CARDINAL; VAR destination: ARRAY OF CHAR) : BOOLEAN;
  (* Returns TRUE if a number of characters, indicated by sourceLength, will fit into
     destination; otherwise returns FALSE.
  *)
BEGIN
   RETURN( sourceLength<=HIGH(destination) )
END CanAssignAll ;


PROCEDURE CanExtractAll (sourceLength,
                         startIndex,
                         numberToExtract: CARDINAL;
                         VAR destination: ARRAY OF CHAR) : BOOLEAN ;
  (* Returns TRUE if there are numberToExtract characters starting at startIndex and
     within the sourceLength of some string, and if the capacity of destination is
     sufficient to hold numberToExtract characters; otherwise returns FALSE.
  *)
BEGIN
   RETURN( (numberToExtract+startIndex<=sourceLength) AND
           (HIGH(destination)>=numberToExtract) )
END CanExtractAll ;

PROCEDURE CanDeleteAll (stringLength, startIndex, numberToDelete: CARDINAL) : BOOLEAN ;
  (* Returns TRUE if there are numberToDelete characters starting at startIndex and
     within the stringLength of some string; otherwise returns FALSE.
  *)
BEGIN
   RETURN( startIndex+numberToDelete<=stringLength )
END CanDeleteAll ;

PROCEDURE CanInsertAll (sourceLength, startIndex: CARDINAL;
                        VAR destination: ARRAY OF CHAR) : BOOLEAN ;
  (* Returns TRUE if there is room for the insertion of sourceLength characters
     from some string into destination starting at startIndex; otherwise returns
     FALSE.
  *)
BEGIN
   RETURN( (HIGH(destination)-Length(destination)<sourceLength) AND
           (HIGH(destination)-startIndex<sourceLength) )
END CanInsertAll ;

PROCEDURE CanReplaceAll (sourceLength, startIndex: CARDINAL;
                         VAR destination: ARRAY OF CHAR) : BOOLEAN;
  (* Returns TRUE if there is room for the replacement of sourceLength
     characters in destination starting at startIndex; otherwise returns
     FALSE.
  *)
BEGIN
   RETURN( sourceLength<=Length(destination)-startIndex )
END CanReplaceAll ;

PROCEDURE CanAppendAll (sourceLength: CARDINAL; VAR destination: ARRAY OF CHAR) : BOOLEAN;
  (* Returns TRUE if there is sufficient room in destination to append a string of
     length sourceLength to the string in destination; otherwise returns FALSE.
  *)
BEGIN
   RETURN( HIGH(destination)-Length(destination)>=sourceLength )
END CanAppendAll ;

PROCEDURE CanConcatAll (source1Length, source2Length: CARDINAL;
                        VAR destination: ARRAY OF CHAR) : BOOLEAN;
  (* Returns TRUE if there is sufficient room in destination for a two strings of
     lengths source1Length and source2Length; otherwise returns FALSE.
  *)
BEGIN
   RETURN( HIGH(destination)-Length(destination)>=source1Length+source2Length )
END CanConcatAll ;


(* The following type and procedures provide for the comparison of string values, and for the
   location of substrings within strings.
*)

PROCEDURE Compare (stringVal1, stringVal2: ARRAY OF CHAR) : CompareResults ;
  (* Returns less, equal, or greater, according as stringVal1 is lexically less than,
     equal to, or greater than stringVal2.
  *)
VAR
   i, l1, l2: CARDINAL ;
BEGIN
   l1 := Length(stringVal1) ;
   l2 := Length(stringVal2) ;
   i := 0 ;
   WHILE (i<l1) AND (i<l2) DO
      IF stringVal1[i]<stringVal2[i]
      THEN
         RETURN less
      ELSIF stringVal1[i]>stringVal2[i]
      THEN
         RETURN greater
      ELSE
         INC(i)
      END
   END ;
   IF l1<l2
   THEN
      RETURN less
   ELSIF l1>l2
   THEN
      RETURN greater
   ELSE
      RETURN equal
   END
END Compare ;

PROCEDURE Equal (stringVal1, stringVal2: ARRAY OF CHAR) : BOOLEAN ;
  (* Returns Strings.Compare(stringVal1, stringVal2) = Strings.equal *)
VAR
   h1, h2, i: CARDINAL ;
   c1, c2   : CHAR ;
BEGIN
   i := 0 ;
   h1 := HIGH(stringVal1) ;
   h2 := HIGH(stringVal2) ;
   IF h1=h2
   THEN
      REPEAT
         c1 := stringVal1[i] ;
         c2 := stringVal2[i] ;
         IF c1#c2
         THEN
            RETURN FALSE
         END ;
         IF c1=ASCII.nul
         THEN
            RETURN TRUE
         END ;
         INC(i) ;
      UNTIL i>h1 ;
      RETURN TRUE
   ELSE
      c1 := stringVal1[0] ;
      c2 := stringVal2[0] ;
      WHILE c1=c2 DO
         IF c1=ASCII.nul
         THEN
            RETURN TRUE
         END ;
         INC(i) ;
         IF i<=h1
         THEN
            c1 := stringVal1[i] ;
            IF i<=h2
            THEN
               c2 := stringVal2[i]
            ELSE
               RETURN c1=ASCII.nul
            END
         ELSIF i<=h2
         THEN
            c2 := stringVal2[i] ;
            RETURN c2=ASCII.nul
         END
      END ;
      RETURN FALSE
   END
END Equal ;

PROCEDURE FindNext (pattern, stringToSearch: ARRAY OF CHAR; startIndex: CARDINAL;
                    VAR patternFound: BOOLEAN; VAR posOfPattern: CARDINAL) ;
  (* Looks forward for next occurrence of pattern in stringToSearch, starting the search at
     position startIndex. If startIndex < LENGTH(stringToSearch) and pattern is found,
     patternFound is returned as TRUE, and posOfPattern contains the start position in
     stringToSearch of pattern. Otherwise patternFound is returned as FALSE, and posOfPattern
     is unchanged.
  *)
VAR
   i, j, hp, hs: CARDINAL ;
BEGIN
   i := startIndex ;
   hp := Length(pattern) ;
   hs := Length(stringToSearch) ;
   IF hp<=hs
   THEN
      WHILE (i<=hs-hp) DO
         j := 0 ;
         WHILE (j<hp) AND (pattern[j]=stringToSearch[i+j]) DO
            INC(j) ;
            IF j=hp
            THEN
               posOfPattern := i ;
               patternFound := TRUE ;
               RETURN
            END
         END ;
         INC(i)
      END
   END ;
   patternFound := FALSE
END FindNext ;

PROCEDURE FindPrev (pattern, stringToSearch: ARRAY OF CHAR; startIndex: CARDINAL;
                    VAR patternFound: BOOLEAN; VAR posOfPattern: CARDINAL);
  (* Looks backward for the previous occurrence of pattern in stringToSearch and returns the
     position of the first character of the pattern if found. The search for the pattern
     begins at startIndex. If pattern is found, patternFound is returned as TRUE, and
     posOfPattern contains the start position in stringToSearch of pattern in the range
     [0..startIndex]. Otherwise patternFound is returned as FALSE, and
     posOfPattern is unchanged.
  *)
VAR
   i, j, hp, hs: CARDINAL ;
BEGIN
   hp := Length(pattern) ;
   hs := Length(stringToSearch) ;
   IF hp<=hs
   THEN
      i := hs-hp+1 ;
      WHILE i>0 DO
         DEC(i) ;
         j := 0 ;
         WHILE (j<hp) AND (pattern[j]=stringToSearch[i+j]) DO
            INC(j) ;
            IF j=hp
            THEN
               posOfPattern := i ;
               patternFound := TRUE ;
               RETURN
            END
         END
      END
   END ;
   patternFound := FALSE
END FindPrev ;

PROCEDURE FindDiff (stringVal1, stringVal2: ARRAY OF CHAR;
                    VAR differenceFound: BOOLEAN; VAR posOfDifference: CARDINAL) ;
  (* Compares the string values in stringVal1 and stringVal2 for differences. If they
     are equal, differenceFound is returned as FALSE, and TRUE otherwise. If
     differenceFound is TRUE, posOfDifference is set to the position of the first
     difference; otherwise posOfDifference is unchanged.
  *)
VAR
   i,
   s1h, s2h: CARDINAL ;
BEGIN
   s1h := Length(stringVal1) ;
   s2h := Length(stringVal2) ;
   i := 0 ;
   WHILE (i<s1h) AND (i<s2h) DO
      IF stringVal1[i]=stringVal2[i]
      THEN
         INC(i)
      ELSE
         differenceFound := TRUE ;
         posOfDifference := i ;
         RETURN
      END
   END ;
   IF s1h=s2h
   THEN
      differenceFound := FALSE ;
   ELSE
      differenceFound := TRUE ;
      posOfDifference := i
   END
END FindDiff ;

PROCEDURE Capitalize (VAR stringVar: ARRAY OF CHAR) ;
  (* Applies the function CAP to each character of the string value in stringVar. *)
VAR
   i, h: CARDINAL ;
BEGIN
   i := 0 ;
   h := Length(stringVar) ;
   WHILE i<h DO
      stringVar[i] := CAP(stringVar[i]) ;
      INC(i)
   END
END Capitalize ;


END Strings.
