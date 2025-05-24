-- C37006A.ADA

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
-- FOR A COMPONENT OF A RECORD, ACCESS, OR PRIVATE TYPE, OR FOR A 
-- LIMITED PRIVATE COMPONENT, CHECK THAT A NON-STATIC EXPRESSION CAN 
-- BE USED IN A DISCRIMINANT CONSTRAINT OR (EXCEPTING LIMITED PRIVATE 
-- COMPONENTS) IN SPECIFYING A DEFAULT INITIAL VALUE.
 
-- R.WILLIAMS 8/28/86
 
WITH REPORT; USE REPORT;
PROCEDURE C37006A IS

     SUBTYPE INT IS INTEGER RANGE 0 .. 100;

     TYPE ARR IS ARRAY (INT RANGE <>) OF INTEGER;

     TYPE REC1 (D1, D2 : INT) IS
          RECORD
               A : ARR (D1 .. D2);
          END RECORD;

     TYPE REC1_NAME IS ACCESS REC1;
 
     PROCEDURE CHECK (AR : ARR; STR : STRING) IS
     BEGIN
          IF AR'FIRST /= 1 OR AR'LAST /= 2 THEN
               FAILED ( "INCORRECT BOUNDS FOR R.COMP.A IN COMPONENT " &
                        "OF " & STR & " TYPE");
          ELSIF AR /= (3, 4) THEN
               FAILED ( "INITIALIZATION OF R.COMP.A IN COMPONENT OF " &
                         STR & " TYPE FAILED" );
          END IF;
     END CHECK;
          
     PACKAGE PACK IS
          TYPE PRIV (D1, D2 : INT) IS PRIVATE;
          TYPE LIM (D1, D2 : INT) IS LIMITED PRIVATE;
          FUNCTION PRIV_FUN (PARM1, PARM2 : INTEGER) RETURN PRIV;
          PROCEDURE PRIV_CHECK (R : PRIV);
          PROCEDURE LIM_CHECK (R : LIM);

     PRIVATE
          TYPE PRIV (D1, D2 : INT) IS
               RECORD
                    A : ARR (D1 .. D2);
               END RECORD;

          TYPE LIM (D1, D2 : INT) IS
               RECORD
                    A : ARR (D1 .. D2);
               END RECORD;
     END PACK;

     PACKAGE BODY PACK IS

          FUNCTION PRIV_FUN (PARM1, PARM2 : INTEGER) RETURN PRIV IS
          BEGIN
               RETURN (IDENT_INT (1), IDENT_INT (2),
                       ARR'(1 => 3, 2 => 4));
          END PRIV_FUN;
          
          PROCEDURE PRIV_CHECK (R : PRIV) IS
          BEGIN
               CHECK (R.A, "PRIVATE TYPE" );
          END PRIV_CHECK;

          PROCEDURE LIM_CHECK (R : LIM) IS
          BEGIN
               IF R.A'FIRST /= 1 OR R.A'LAST /= 2 THEN
                    FAILED ( "INCORRECT BOUNDS FOR R.COMP.A IN " &
                             "COMPONENT OF LIMITED PRIVATE TYPE");
               END IF;
          END LIM_CHECK;
     END PACK;
 
     USE PACK;

BEGIN
     
     TEST ( "C37006A", "FOR A COMPONENT OF A RECORD, ACCESS, " &
                       "OR PRIVATE TYPE, OR FOR A LIMITED PRIVATE " &
                       "COMPONENT, CHECK THAT A NON-STATIC " &
                       "EXPRESSION CAN BE USED IN A DISCRIMINANT " &
                       "CONSTRAINT OR (EXCEPTING LIMITED PRIVATE " &
                       "COMPONENTS) IN SPECIFYING A DEFAULT " &
                       "INITIAL VALUE" );
 
     BEGIN
          DECLARE
          
               TYPE REC2 IS
                    RECORD
                         COMP : REC1 (IDENT_INT (1), IDENT_INT (2)) := 
                                (IDENT_INT (1), IDENT_INT (2), 
                                 ARR'(1 => 3, 2 => 4));
                    END RECORD;
 
          R : REC2;

          BEGIN
               IF R.COMP.D1 = 1 AND R.COMP.D2 = 2 THEN 
                    CHECK (R.COMP.A, "RECORD");
               ELSE 
                    FAILED ( "INCORRECT VALUE FOR DISCRIMINANTS " &
                             "OF RECORD TYPE COMPONENT" );
               END IF;

          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    FAILED ( "CONSTRAINT_ERROR RAISED IN STATEMENT " &
                             "SEQUENCE FOLLOWING DECLARATION OF " &
                             "RECORD TYPE COMPONENT" );
               WHEN OTHERS =>
                    FAILED ( "OTHER EXCEPTION RAISED IN STATEMENT " &
                             "SEQUENCE FOLLOWING DECLARATION OF " &
                             "RECORD TYPE COMPONENT" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ( "CONSTRAINT_ERROR RAISED BY DECLARATION " &
                        "OF RECORD TYPE COMPONENT" );
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED BY DECLARATION " &
                        "OF RECORD TYPE COMPONENT" );
     END;
 
     BEGIN
          DECLARE
          
               TYPE REC2 IS
                    RECORD
                         COMP : REC1_NAME (IDENT_INT (1), 
                                           IDENT_INT (2)) := 
                                NEW REC1'(IDENT_INT (1), 
                                          IDENT_INT (2), 
                                          ARR'(1 => 3, 2 => 4));
                    END RECORD;
 
          R : REC2;

          BEGIN
               IF R.COMP.D1 = 1 AND R.COMP.D2 = 2 THEN 
                    CHECK (R.COMP.A, "ACCESS");
               ELSE 
                    FAILED ( "INCORRECT VALUE FOR DISCRIMINANTS " &
                             "OF ACCESS TYPE COMPONENT" );
               END IF;

          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    FAILED ( "CONSTRAINT_ERROR RAISED IN STATEMENT " &
                             "SEQUENCE FOLLOWING DECLARATION OF " &
                             "ACCESS TYPE COMPONENT" );
               WHEN OTHERS =>
                    FAILED ( "OTHER EXCEPTION RAISED IN STATEMENT " &
                             "SEQUENCE FOLLOWING DECLARATION OF " &
                             "ACCESS TYPE COMPONENT" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ( "CONSTRAINT_ERROR RAISED BY DECLARATION " &
                        "OF ACCESS TYPE COMPONENT" );
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED BY DECLARATION " &
                        "OF ACCESS TYPE COMPONENT" );
     END;
 
     BEGIN
          DECLARE
          
               TYPE REC2 IS
                    RECORD
                         COMP : PRIV (IDENT_INT (1), IDENT_INT (2)) := 
                                PRIV_FUN (IDENT_INT (1), 
                                          IDENT_INT (2));
                    END RECORD;
 
          R : REC2;

          BEGIN
               IF R.COMP.D1 = 1 AND R.COMP.D2 = 2 THEN 
                    PRIV_CHECK (R.COMP);
               ELSE 
                    FAILED ( "INCORRECT VALUE FOR DISCRIMINANTS " &
                             "OF PRIVATE TYPE COMPONENT" );
               END IF;

          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    FAILED ( "CONSTRAINT_ERROR RAISED IN STATEMENT " &
                             "SEQUENCE FOLLOWING DECLARATION OF " &
                             "PRIVATE TYPE COMPONENT" );
               WHEN OTHERS =>
                    FAILED ( "OTHER EXCEPTION RAISED IN STATEMENT " &
                             "SEQUENCE FOLLOWING DECLARATION OF " &
                             "PRIVATE TYPE COMPONENT" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ( "CONSTRAINT_ERROR RAISED BY DECLARATION " &
                        "OF PRIVATE TYPE COMPONENT" );
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED BY DECLARATION " &
                        "OF PRIVATE TYPE COMPONENT" );
     END;
 
     BEGIN
          DECLARE
          
               TYPE REC2 IS
                    RECORD
                         COMP : LIM (IDENT_INT (1), IDENT_INT (2));
                    END RECORD;

          R : REC2;

          BEGIN
               IF R.COMP.D1 = 1 AND R.COMP.D2 = 2 THEN
                    LIM_CHECK (R.COMP);
               ELSE
                    FAILED ( "INCORRECT VALUE FOR DISCRIMINANTS " &
                             "OF LIM PRIV TYPE COMPONENT" );
               END IF;

          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    FAILED ( "CONSTRAINT_ERROR RAISED IN STATEMENT " &
                             "SEQUENCE FOLLOWING DECLARATION OF " &
                             " LIM PRIV TYPE COMPONENT" );
               WHEN OTHERS =>
                    FAILED ( "OTHER EXCEPTION RAISED IN STATEMENT " &
                             "SEQUENCE FOLLOWING DECLARATION OF " &
                             " LIM PRIV TYPE COMPONENT" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ( "CONSTRAINT_ERROR RAISED BY DECLARATION " &
                        "OF  LIM PRIV TYPE COMPONENT" );
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED BY DECLARATION " &
                        "OF  LIM PRIV TYPE COMPONENT" );
     END;
 
     RESULT;
 
END C37006A;
