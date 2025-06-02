-- C37310A.ADA

--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained 
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making 
--     this public release, the Government intends to confer upon all 
--     recipients unlimited rights  equal to those held by the Government.  
--     These rights include rights to use, duplicate, release or disclose the 
--     released technical data and computer software in whole or in part, in 
--     any manner and for any purpose whatsoever, and to have or permit others 
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED 
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE 
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
-- CHECK THAT IF A DISCRIMINANT HAS A DYNAMIC SUBTYPE, AN OTHERS
-- CHOICE CAN BE OMITTED IF ALL VALUES IN THE BASE
-- TYPE'S RANGE ARE COVERED.
 
-- ASL 7/10/81
-- SPS 10/25/82
-- PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH REPORT;
PROCEDURE C37310A IS
 
     USE REPORT;
 
BEGIN
     TEST ("C37310A", "CHECK DYNAMIC DISCRIMINANT SUBTYPES " &
                      "IN VARIANT RECORD DECLARATIONS");

     DECLARE
 
          ACHAR : CHARACTER := IDENT_CHAR('A');
          ECHAR : CHARACTER := IDENT_CHAR('E');
          JCHAR : CHARACTER := IDENT_CHAR('J');
          MCHAR : CHARACTER := IDENT_CHAR('M');
          SUBTYPE STATCHAR IS CHARACTER RANGE 'I'..'N';
          SUBTYPE DYNCHAR IS CHARACTER RANGE ACHAR..ECHAR; 
          SUBTYPE SSTAT IS STATCHAR RANGE JCHAR..MCHAR;

          TYPE LETTER IS NEW CHARACTER RANGE 'A'..'Z';
          SUBTYPE DYNLETTER IS 
               LETTER RANGE LETTER(ECHAR)..LETTER(JCHAR);     
 
          TYPE REC1(DISC : SSTAT := 'K') IS
               RECORD
                    CASE DISC IS
                         WHEN ASCII.NUL..CHARACTER'LAST => NULL;
                    END CASE;
               END RECORD;
      
          TYPE REC2(DISC : DYNCHAR := 'C') IS
               RECORD
                    CASE DISC IS
                         WHEN ASCII.NUL..CHARACTER'LAST => NULL;
                    END CASE;       
               END RECORD;
      
          TYPE REC3(DISC: DYNCHAR := 'D') IS
               RECORD
                    CASE DISC IS
                         WHEN CHARACTER'FIRST..CHARACTER'LAST => NULL;
                    END CASE; 
               END RECORD;
      
          TYPE REC4(DISC : DYNLETTER := 'F') IS
               RECORD
                    CASE DISC IS
                         WHEN LETTER'BASE'FIRST..
                              LETTER'BASE'LAST => NULL;
                    END CASE; 
               END RECORD;
 
          R1 : REC1;
          R2 : REC2;
          R3 : REC3;
          R4 : REC4;
     BEGIN
          IF EQUAL(3,3) THEN
               R1 := (DISC => 'L');
          END IF;
          IF R1.DISC /= 'L' THEN
               FAILED ("ASSIGNMENT FAILED - 1");
          END IF;
 
          IF EQUAL(3,3) THEN
              R2 := (DISC => 'B');
          END IF;
          IF R2.DISC /= 'B' THEN
               FAILED ("ASSIGNMENT FAILED - 2");
          END IF;
 
          IF EQUAL(3,3) THEN
               R3 := (DISC => 'B');
          END IF;
          IF R3.DISC /= 'B' THEN
               FAILED ("ASSIGNMENT FAILED - 3");
          END IF;
 
          IF EQUAL(3,3) THEN
               R4 := (DISC => 'H');
          END IF;
          IF R4.DISC /= 'H' THEN
               FAILED ("ASSIGNMENT FAILED - 4");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
              FAILED ("EXCEPTION RAISED");
     END;
 
     RESULT;
 
END C37310A;
