-- C37108B.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED IN AN OBJECT DECLARATION IF
-- A DEFAULT INITIAL VALUE HAS BEEN SPECIFIED WHICH VIOLATES THE 
-- CONSTRAINTS OF A RECORD OR AN ARRAY TYPE WHOSE CONSTRAINT
-- DEPENDS ON A DISCRIMINANT, AND NO EXPLICIT INITIALIZATION IS
-- PROVIDED FOR THE OBJECT.

-- R.WILLIAMS 8/25/86
-- EDS        7/16/98    AVOID OPTIMIZATION

WITH REPORT; USE REPORT;
PROCEDURE C37108B IS
     
     TYPE ARR IS ARRAY (POSITIVE RANGE <>) OF INTEGER;

     TYPE R (P : POSITIVE) IS
          RECORD
               NULL;
          END RECORD;
     
BEGIN
     TEST ( "C37108B", "CHECK THAT CONSTRAINT_ERROR IS RAISED IN " &
                       "AN OBJECT DECLARATION IF A DEFAULT INITIAL " &
                       "VALUE HAS BEEN SPECIFIED WHICH VIOLATES THE " &
                       "CONSTRAINTS OF A RECORD OR AN ARRAY TYPE " &
                       "WHOSE CONSTRAINT DEPENDS ON A DISCRIMINANT, " &
                       "AND NO EXPLICIT INITIALIZATION IS PROVIDED " &
                       "FOR THE OBJECT" );


     BEGIN
          DECLARE     
               TYPE REC1 (D : NATURAL := IDENT_INT (0)) IS
                    RECORD
                         A : ARR (D .. 5);
                    END RECORD;
               
          BEGIN
               DECLARE 
                    R1 : REC1;
                    
               BEGIN
                    R1.A (1) := IDENT_INT (2);
                    FAILED ( "NO EXCEPTION RAISED AT DECLARATION OF " &
                             "R1" & INTEGER'IMAGE(R1.A(5)));  --USE R2
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ( "EXCEPTION FOR R1 RAISED INSIDE " &
                                  "BLOCK" );
               END;
     
          EXCEPTION
               WHEN CONSTRAINT_ERROR => 
                    NULL;
               WHEN OTHERS =>
                    FAILED ( "WRONG EXCEPTION RAISED AT DECLARATION " &
                             "OF R1" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ( "CONSTRAINT_ERROR RAISED FOR TYPE " &
                        "DECLARATION OF REC1" );
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED FOR TYPE " &
                        "DECLARATION OF REC1" );
     END;
     
     BEGIN
          DECLARE     
               TYPE REC2 (D : INTEGER := IDENT_INT (-1)) IS
                    RECORD
                         A : R (P => D);
                    END RECORD;
               
          BEGIN
               DECLARE 
                    R2 : REC2;
                    
               BEGIN
                    R2.A := R'(P => IDENT_INT (1));
                    FAILED ( "NO EXCEPTION RAISED AT DECLARATION OF " &
                             "R2" & INTEGER'IMAGE(R2.A.P));  --USE R2
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ( "EXCEPTION FOR R2 RAISED INSIDE " &
                                  "BLOCK" );
               END;
     
          EXCEPTION
               WHEN CONSTRAINT_ERROR => 
                    NULL;
               WHEN OTHERS =>
                    FAILED ( "WRONG EXCEPTION RAISED AT DECLARATION " &
                             "OF R2" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ( "CONSTRAINT_ERROR RAISED FOR TYPE " &
                        "DECLARATION OF REC2" );
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED FOR TYPE " &
                        "DECLARATION OF REC2" );
     END;

     BEGIN
          DECLARE     
               PACKAGE PRIV IS
                    TYPE REC3 (D : INTEGER := IDENT_INT (-1)) IS 
                         PRIVATE;
                    PROCEDURE PROC (R :REC3);
                         
               PRIVATE
                    TYPE REC3 (D : INTEGER := IDENT_INT (-1)) IS
                         RECORD
                              A : R (P => D);
                         END RECORD;
               END PRIV;
               
               PACKAGE BODY PRIV IS
                    PROCEDURE PROC (R : REC3) IS
                         I : INTEGER;
                    BEGIN
                         I := IDENT_INT (R.A.P);
                         IF EQUAL(2, IDENT_INT(1)) THEN
                              FAILED("IMPOSSIBLE " & INTEGER'IMAGE(I));  --USE I
                         END IF;
                    END PROC;
               END PRIV;

               USE PRIV;
               
          BEGIN
               DECLARE 
                    R3 : REC3;
                    
               BEGIN
                    PROC (R3);
                    FAILED ( "NO EXCEPTION RAISED AT " &
                              "DECLARATION OF R3" );
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ( "EXCEPTION FOR R3 RAISED INSIDE " &
                                  "BLOCK" );
               END;
     
          EXCEPTION
               WHEN CONSTRAINT_ERROR => 
                    NULL;
               WHEN OTHERS =>
                    FAILED ( "WRONG EXCEPTION RAISED AT DECLARATION " &
                             "OF R3" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ( "CONSTRAINT_ERROR RAISED FOR TYPE " &
                        "DECLARATION OF REC3" );
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED FOR TYPE " &
                        "DECLARATION OF REC3" );
     END;

     BEGIN
          DECLARE     
               PACKAGE LPRIV IS
                    TYPE REC4 (D : NATURAL := IDENT_INT (0))
                         IS LIMITED PRIVATE;
                    PROCEDURE PROC (R :REC4);
                         
               PRIVATE
                    TYPE REC4 (D : NATURAL := IDENT_INT (0)) IS
                         RECORD
                              A : ARR (D .. 5);
                         END RECORD;
               END LPRIV;
               
               PACKAGE BODY LPRIV IS
                    PROCEDURE PROC (R : REC4) IS
                         I : INTEGER;
                    BEGIN
                         I := IDENT_INT (R.A'FIRST);
                         IF EQUAL(2, IDENT_INT(1)) THEN
                              FAILED("IMPOSSIBLE " & INTEGER'IMAGE(I));  --USE I
                         END IF;
                    END PROC;
               END LPRIV;

               USE LPRIV;
               
          BEGIN
               DECLARE 
                    R4 : REC4;
                    
               BEGIN
                    PROC (R4);
                    FAILED ( "NO EXCEPTION RAISED AT " &
                             "DECLARATION OF R4" );
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ( "EXCEPTION FOR R4 RAISED INSIDE " &
                                  "BLOCK" );
               END;
     
          EXCEPTION
               WHEN CONSTRAINT_ERROR => 
                    NULL;
               WHEN OTHERS =>
                    FAILED ( "WRONG EXCEPTION RAISED AT DECLARATION " &
                             "OF R4" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ( "CONSTRAINT_ERROR RAISED FOR TYPE " &
                        "DECLARATION OF REC4" );
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED FOR TYPE " &
                        "DECLARATION OF REC4" );
     END;

     RESULT;
END C37108B;
