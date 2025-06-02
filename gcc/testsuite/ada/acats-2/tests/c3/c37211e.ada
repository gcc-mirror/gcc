-- C37211E.ADA

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
-- OF THE DISCRIMINANT. 

-- R.WILLIAMS 8/28/86
-- PWN 10/27/95  REMOVED CHECK WHERE CONSTRAINT RULES HAVE CHANGED.
-- PWN 12/03/95  CORRECTED FORMATING PROBLEM.
-- TMB 11/20/96  REINTRODUCED CHECK REMOVED ON 10/27 WITH ADA95 CHANGES
-- TMB 12/2/96   DELETED CHECK OF CONSTRAINED ACCESS TYPE
-- EDS 07/14/98  AVOID OPTIMIZATION

WITH REPORT; USE REPORT;
PROCEDURE C37211E IS

     TYPE REC (D : POSITIVE) IS
          RECORD
               NULL;
          END RECORD;
     
     TYPE ACC IS ACCESS REC;
BEGIN
     TEST ( "C37211E", "CHECK THAT CONSTRAINT_ERROR IS RAISED BY " &
                       "A DISCRIMINANT CONSTRAINT IF A VALUE " &
                       "SPECIFIED FOR A DISCRIMINANT DOES NOT LIE " &
                       "IN THE RANGE OF THE DISCRIMINANT WHERE THE " &
                       "TYPE MARK DENOTES AN ACCESS TYPE" );
                         
     BEGIN
          DECLARE
               SUBTYPE SUBACC IS ACC (IDENT_INT (-1));
          BEGIN
               DECLARE
                    SA : SUBACC;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF SUBTYPE SUBACC " & 
                             INTEGER'IMAGE(SA.D));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT SA" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "SUBTYPE SUBACC" );
     END;

     BEGIN
          DECLARE
               TYPE ARR IS ARRAY (1 .. 10) OF ACC (IDENT_INT (-1));
          BEGIN
               DECLARE
                    AR : ARR;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF TYPE ARR " & 
                             INTEGER'IMAGE(AR(1).D));
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
                         X : ACC (IDENT_INT (-1));
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
               TYPE ACCA IS ACCESS ACC (IDENT_INT (-1));
          BEGIN
               DECLARE
                    ACA : ACCA;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF TYPE ACCA " & 
                             INTEGER'IMAGE(ACA.ALL.D));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT ACA" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "TYPE ACCA" );
     END;
          
     BEGIN
          DECLARE
               TYPE NEWACC IS NEW ACC (IDENT_INT (-1));
          BEGIN
               DECLARE
                    NA : NEWACC;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF TYPE NEWACC " & 
                             INTEGER'IMAGE(NA.D));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT NA" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "TYPE NEWACC" );
     END;

     BEGIN
          DECLARE
               A : ACC (IDENT_INT (-1));
          BEGIN
               FAILED ( "NO EXCEPTION RAISED AT THE DECLARATION OF " &
                        "A " & INTEGER'IMAGE(A.D));
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED INSIDE BLOCK " &
                             "CONTAINING A" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT DECLARATION OF " &
                        "A" );
     END;


     BEGIN
          DECLARE
               TYPE BAD_ACC (D : POSITIVE := IDENT_INT (-1)) IS
                    RECORD
                         NULL;
                    END RECORD;
          BEGIN
               DECLARE
                    BAC : BAD_ACC;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "DECLARATION OF OBJECT BAC " & 
                             INTEGER'IMAGE(BAC.D));
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ( "EXCEPTION RAISED INSIDE BLOCK " &
                                  "DECLARING BAC" );
               END;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;               
               WHEN OTHERS => 
                    FAILED ( "WRONG EXCEPTION RAISED AT DECLARATION " &
                             "OF OBJECT BAC" );
          END;
     EXCEPTION
          WHEN OTHERS => 
               FAILED ( "EXCEPTION RAISED AT ELABORATION OF TYPE " &
                        "BAD_ACC" );
     END;

     RESULT;
END C37211E;
