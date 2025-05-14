*> { dg-do run }
*> { dg-output {0    is a hexadecimal number(\n|\r\n|\r)} }
*> { dg-output {Dead is a hexadecimal number(\n|\r\n|\r)} }
*> { dg-output {Fred is not a hexadecimal number(\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output {0               is not a real name(\n|\r\n|\r)} }
*> { dg-output {Dead            is a real name(\n|\r\n|\r)} }
*> { dg-output {Fred            is a real name(\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output {0               is not alphabetic(\n|\r\n|\r)} }
*> { dg-output {Dead            is alphabetic(\n|\r\n|\r)} }
*> { dg-output {Fred            is alphabetic(\n|\r\n|\r)} }
*> { dg-output { } }
IDENTIFICATION DIVISION.
PROGRAM-ID. test.
AUTHOR. Michael Coughlan.
*> This routine is based on Listing-5-1
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SPECIAL-NAMES.
    CLASS HexNumber IS "0" THRU "9", "A" THRU "F", "a" THRU "f", SPACE
    CLASS RealName  IS "A" THRU "Z", "a" THRU "z", "'", SPACE.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 NumIn       PIC X(4).
01 NameIn      PIC X(15).
PROCEDURE DIVISION.
    MOVE "0" TO NumIn
    PERFORM TestHex.
    MOVE "Dead" TO NumIn
    PERFORM TestHex.
    MOVE "Fred" TO NumIn
    PERFORM TestHex.
    DISPLAY " "
    MOVE "0" TO NameIn
    PERFORM TestRealname
    MOVE "Dead" TO NameIn
    PERFORM TestRealname
    MOVE "Fred" TO NameIn
    PERFORM TestRealname
    DISPLAY " "
    MOVE "0" TO NameIn
    PERFORM TestAlphabetic
    MOVE "Dead" TO NameIn
    PERFORM TestAlphabetic
    MOVE "Fred" TO NameIn
    PERFORM TestAlphabetic
    DISPLAY " "
    STOP RUN.
TestRealname.
   IF NameIn IS RealName THEN
      DISPLAY NameIn " is a real name"
    ELSE
      DISPLAY NameIn " is not a real name"
   END-IF.
TestHex.
   IF NumIn IS HexNumber THEN
      DISPLAY NumIn " is a hexadecimal number"
    ELSE
      DISPLAY NumIn " is not a hexadecimal number"
   END-IF.
TestAlphabetic.
   IF NameIn IS ALPHABETIC
      DISPLAY NameIn " is alphabetic"
    ELSE
      DISPLAY NameIn " is not alphabetic"
   END-IF.
  END PROGRAM test.
