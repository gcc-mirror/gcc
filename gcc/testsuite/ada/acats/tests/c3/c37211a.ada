-- C37211A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED BY A DISCRIMINANT CONSTRAINT 
-- IF A VALUE SPECIFIED FOR A DISCRIMINANT DOES NOT LIE IN THE RANGE
-- OF THE DISCRIMINANT. THIS TEST CONTAINS CHECKS FOR SUBTYPE 
-- INDICATIONS WHERE THE TYPE MARK DENOTES A RECORD TYPE.

-- R.WILLIAMS 8/28/86
-- EDS        7/14/98    AVOID OPTIMIZATION

WITH REPORT; USE REPORT;
PROCEDURE C37211A IS

     TYPE REC (D : POSITIVE) IS
          RECORD
               NULL;
          END RECORD;
     
BEGIN
     TEST ( "C37211A", "CHECK THAT CONSTRAINT_ERROR IS RAISED BY " &
                       "A DISCRIMINANT CONSTRAINT IF A VALUE " &
                       "SPECIFIED FOR A DISCRIMINANT DOES NOT LIE " &
                       "IN THE RANGE OF THE DISCRIMINANT WHERE THE " &
                       "TYPE MARK DENOTES A RECORD TYPE" );
                         
     BEGIN
          DECLARE
               SUBTYPE SUBREC IS REC (IDENT_INT (-1));
          BEGIN
               DECLARE
                    SR : SUBREC;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF SUBTYPE SUBREC " & INTEGER'IMAGE(SR.D));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT SR" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "SUBTYPE SUBREC" );
     END;

     BEGIN
          DECLARE
               TYPE ARR IS ARRAY (1 .. 10) OF REC (IDENT_INT (-1));
          BEGIN
               DECLARE
                    AR : ARR;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF TYPE ARR " & INTEGER'IMAGE(AR(1).D));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT AR" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "TYPE ARR" );
     END;
          
     BEGIN
          DECLARE
               TYPE REC1 IS 
                    RECORD
                         X : REC (IDENT_INT (-1));
                    END RECORD;

          BEGIN
               DECLARE
                    R1 : REC1;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF TYPE REC1 " & INTEGER'IMAGE(R1.X.D));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT R1" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "TYPE REC1" );
     END;
          
     BEGIN
          DECLARE
               TYPE ACCREC IS ACCESS REC (IDENT_INT (-1));
          BEGIN
               DECLARE
                    ACR : ACCREC;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF TYPE ACCREC " & INTEGER'IMAGE(ACR.D));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT ACR" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "TYPE ACCREC" );
     END;
          
     BEGIN
          DECLARE
               TYPE NEWREC IS NEW REC (IDENT_INT (-1));
          BEGIN
               DECLARE
                    NR : NEWREC;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF TYPE NEWREC " & INTEGER'IMAGE(NR.D));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT NR" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "TYPE NEWREC" );
     END;

     BEGIN
          DECLARE
               R : REC (IDENT_INT (-1));
          BEGIN
               FAILED ( "NO EXCEPTION RAISED AT THE DECLARATION OF " &
                        "R " & INTEGER'IMAGE(R.D));
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED INSIDE BLOCK " &
                             "CONTAINING R" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT DECLARATION OF " &
                        "R" );
     END;

     BEGIN
          DECLARE
               TYPE REC_NAME IS ACCESS REC;
          BEGIN
               DECLARE
                    RN : REC_NAME := NEW REC (IDENT_INT (-1));
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "DECLARATION OF OBJECT RN " & INTEGER'IMAGE(RN.D));
               END;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;               
               WHEN OTHERS => 
                    FAILED ( "WRONG EXCEPTION RAISED AT DECLARATION " &
                             "OF OBJECT RN" );
          END;
     EXCEPTION
          WHEN OTHERS => 
               FAILED ( "EXCEPTION RAISED AT ELABORATION OF TYPE " &
                        "REC_NAME" );
     END;

     BEGIN
          DECLARE
               TYPE BAD_REC (D : POSITIVE := IDENT_INT (-1)) IS
                    RECORD
                         NULL;
                    END RECORD;
          BEGIN
               DECLARE
                    BR : BAD_REC;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "DECLARATION OF OBJECT BR " & INTEGER'IMAGE(BR.D));
               END;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;               
               WHEN OTHERS => 
                    FAILED ( "WRONG EXCEPTION RAISED AT DECLARATION " &
                             "OF OBJECT BR" );
          END;
     EXCEPTION
          WHEN OTHERS => 
               FAILED ( "EXCEPTION RAISED AT ELABORATION OF TYPE " &
                        "BAD_REC" );
     END;

     RESULT;
END C37211A;
