-- C41301A.ADA

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
-- CHECK THAT THE NOTATION L.R MAY BE USED TO DENOTE A RECORD COMPONENT,
--   WHERE R IS THE IDENTIFIER OF SUCH COMPONENT, AND L MAY BE ANY OF
--   THE FOLLOWING:
--        AN IDENTIFIER DENOTING A RECORD OBJECT - X2;
--        AN IDENTIFIER DENOTING AN ACCESS OBJECT WHOSE VALUE DESIGNATES
--           A RECORD OBJECT - X3;
--        A FUNCTION CALL DELIVERING A RECORD VALUE - F1;
--        A FUNCTION CALL DELIVERING AN ACCESS VALUE DESIGNATING A
--           RECORD OBJECT - F2;
--        AN INDEXED COMPONENT - X4;
--        AN IDENTIFIER PREFIXED BY THE NAME OF THE INNERMOST UNIT
--           ENCLOSING THE IDENTIFIER'S DECLARATION - C41301A.X1;
--        A SELECTED COMPONENT DENOTING A RECORD (WHICH IS A COMPONENT
--           OF ANOTHER RECORD) - X5.

-- WKB 8/13/81
-- JRK 8/17/81
-- SPS 10/26/82

WITH REPORT;
USE REPORT;
PROCEDURE C41301A IS

     TYPE T1 IS
          RECORD
               A : INTEGER;
               B : BOOLEAN;
               C : BOOLEAN;
          END RECORD;
     X1 : T1 := (A=>1, B=>TRUE, C=>FALSE);

BEGIN
     TEST ("C41301A", "CHECK THAT THE NOTATION L.R MAY BE USED TO " &
                      "DENOTE A RECORD COMPONENT, WHERE R IS THE " &
                      "IDENTIFIER AND L MAY BE OF CERTAIN FORMS");

     DECLARE

          TYPE T2 (DISC : INTEGER := 0) IS
               RECORD
                    D : BOOLEAN;
                    E : INTEGER;
                    F : BOOLEAN;
                    CASE DISC IS
                         WHEN 1 =>
                              G : BOOLEAN;
                         WHEN 2 =>
                              H : INTEGER;
                         WHEN OTHERS =>
                              NULL;
                    END CASE;
               END RECORD;
          X2 : T2(2) := (DISC=>2, D=>TRUE, E=>3, F=>FALSE, H=>1);

          TYPE T3 IS ACCESS T1;
          X3 : T3 := NEW T1' (A=>1, B=>TRUE, C=>FALSE);

          TYPE T4 IS ARRAY (1..3) OF T1;
          X4 : T4 := (1 => (1, TRUE, FALSE),
                      2 => (2, FALSE, TRUE),
                      3 => (3, TRUE, FALSE));

          TYPE T5 IS
               RECORD
                    I : INTEGER;
                    J : T1;
               END RECORD;
          X5 : T5 := (I => 5, J => (6, FALSE, TRUE));

          FUNCTION F1 RETURN T2 IS
          BEGIN
               RETURN (DISC=>1, D=>FALSE, E=>3, F=>TRUE, G=>FALSE);
          END F1;

          FUNCTION F2 RETURN T3 IS
          BEGIN
               RETURN X3;
          END F2;

          PROCEDURE P1 (X : IN BOOLEAN; Y : IN OUT INTEGER;
                        Z : OUT BOOLEAN; W : STRING) IS
          BEGIN
               IF X /= TRUE THEN
                    FAILED ("WRONG VALUE FOR IN PARAMETER - " & W);
               END IF;
               IF Y /= 1 THEN
                    FAILED ("WRONG VALUE FOR IN OUT PARAMETER - " & W);
               END IF;
               Y := 10;
               Z := TRUE;
          END P1;

          PROCEDURE P2 (X : IN INTEGER) IS
          BEGIN
               IF X /= 1 THEN
                    FAILED ("WRONG VALUE FOR IN PARAMETER - F1");
               END IF;
          END P2;

     BEGIN

          IF X2.E /= 3 THEN
               FAILED ("WRONG VALUE FOR EXPRESSION - X2");
          END IF;
          X2.E := 5;
          IF X2 /= (2, TRUE, 5, FALSE, 1) THEN
               FAILED ("WRONG TARGET FOR ASSIGNMENT - X2");
          END IF;
          X2 := (DISC=>2, D=>TRUE, E=>3, F=>FALSE, H=>1);
          P1 (X2.D, X2.H, X2.F, "X2");
          IF X2 /= (2, TRUE, 3, TRUE, 10) THEN
               FAILED ("WRONG TARGET FOR (IN) OUT PARAMETER - X2");
          END IF;

          IF X3.C /= FALSE THEN
               FAILED ("WRONG VALUE FOR EXPRESSION - X3");
          END IF;
          X3.A := 5;
          IF X3.ALL /= (5, TRUE, FALSE) THEN
               FAILED ("WRONG TARGET FOR ASSIGNMENT - X3");
          END IF;
          X3 := NEW T1 '(A=>1, B=>TRUE, C=>FALSE);
          P1 (X3.B, X3.A, X3.C, "X3");
          IF X3.ALL /= (10, TRUE, TRUE) THEN
               FAILED ("WRONG TARGET FOR (IN) OUT PARAMETER - X3");
          END IF;

          IF F1.G /= FALSE THEN
               FAILED ("WRONG VALUE FOR EXPRESSION - F1");
          END IF;
          P2 (F1.DISC);

          X3 := NEW T1' (A=>3, B=>FALSE, C=>TRUE);
          IF F2.B /= FALSE THEN
               FAILED ("WRONG VALUE FOR EXPRESSION - F2");
          END IF;
          F2.A := 4;
          IF X3.ALL /= (4, FALSE, TRUE) THEN
               FAILED ("WRONG TARGET FOR ASSIGNMENT - F2");
          END IF;
          X3 := NEW T1' (A=>1, B=>FALSE, C=>TRUE);
          P1 (F2.C, F2.A, F2.B, "F2");
          IF X3.ALL /= (10, TRUE, TRUE) THEN
               FAILED ("WRONG TARGET FOR (IN) OUT PARAMETER - F2");
          END IF;

          IF X4(2).C /= TRUE THEN
               FAILED ("WRONG VALUE FOR EXPRESSION - X4");
          END IF;
          X4(3).A := 4;
          IF X4 /= ((1,TRUE,FALSE), (2,FALSE,TRUE), (4,TRUE,FALSE)) THEN
               FAILED ("WRONG TARGET FOR ASSIGNMENT - X4");
          END IF;
          X4 := (1 => (2,TRUE,FALSE), 2 => (1,FALSE,TRUE),
                 3 => (3,TRUE,FALSE));
          P1 (X4(3).B, X4(2).A, X4(1).C, "X4");
          IF X4 /= ((2,TRUE,TRUE), (10,FALSE,TRUE), (3,TRUE,FALSE)) THEN
               FAILED ("WRONG TARGET FOR (IN) OUT PARAMETER - X4");
          END IF;

          X1 := (A=>1, B=>FALSE, C=>TRUE);
          IF C41301A.X1.C /= TRUE THEN
               FAILED ("WRONG VALUE FOR EXPRESSION - C41301A.X1");
          END IF;
          C41301A.X1.B := TRUE;
          IF X1 /= (1, TRUE, TRUE) THEN
               FAILED ("WRONG TARGET FOR ASSIGNMENT - C41301A.X1");
          END IF;
          X1 := (A=>1, B=>FALSE, C=>TRUE);
          P1 (C41301A.X1.C, C41301A.X1.A, C41301A.X1.B, "C41301A.X1");
          IF X1 /= (10, TRUE, TRUE) THEN
               FAILED ("WRONG TARGET FOR (IN) OUT PARAMETER - " &
                       "C41301A.X1");
          END IF;

          IF X5.J.C /= TRUE THEN
               FAILED ("WRONG VALUE FOR EXPRESSION - X5");
          END IF;
          X5.J.C := FALSE;
          IF X5 /= (5, (6, FALSE, FALSE)) THEN
               FAILED ("WRONG TARGET FOR ASSIGNMENT - X5");
          END IF;
          X5 := (I => 5, J => (A=>1, B=>TRUE, C=>FALSE));
          P1 (X5.J.B, X5.J.A, X5.J.C, "X5");
          IF X5 /= (5, (10, TRUE, TRUE)) THEN
               FAILED ("WRONG TARGET FOR (IN) OUT PARAMETER - X5");
          END IF;

     END;

     RESULT;
END C41301A;
