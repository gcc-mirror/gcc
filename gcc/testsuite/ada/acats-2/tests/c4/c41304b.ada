-- C41304B.ADA

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
-- OBJECTIVE:
--      CHECK THAT L.R RAISES CONSTRAINT_ERROR WHEN:
--        L DENOTES A RECORD OBJECT SUCH THAT, FOR THE EXISTING
--           DISCRIMINANT VALUES, THE COMPONENT DENOTED BY R DOES
--           NOT EXIST.
--        L IS A FUNCTION CALL DELIVERING A RECORD VALUE SUCH THAT,
--           FOR THE EXISTING DISCRIMINANT VALUES, THE COMPONENT
--           DENOTED BY R DOES NOT EXIST.
--        L IS AN ACCESS OBJECT AND THE OBJECT DESIGNATED BY THE ACCESS
--           VALUE IS SUCH THAT COMPONENT R DOES NOT EXIST FOR THE
--           OBJECT'S CURRENT DISCRIMINANT VALUES.
--        L IS A FUNCTION CALL RETURNING AN ACCESS VALUE AND THE OBJECT
--           DESIGNATED BY THE ACCESS VALUE IS SUCH THAT COMPONENT R
--           DOES NOT EXIST FOR THE OBJECT'S CURRENT DISCRIMINANT
--           VALUES.

-- HISTORY:
--     TBN 05/23/86  CREATED ORIGINAL TEST.
--     JET 01/08/88  MODIFIED HEADER FORMAT AND ADDED CODE TO
--                   PREVENT OPTIMIZATION.

WITH REPORT; USE REPORT;
PROCEDURE C41304B IS

     TYPE V (DISC : INTEGER := 0) IS
          RECORD
               CASE DISC IS
                    WHEN 1 =>
                         X : INTEGER;
                    WHEN OTHERS =>
                         Y : INTEGER;
               END CASE;
          END RECORD;

     TYPE T IS ACCESS V;

BEGIN
     TEST ("C41304B", "CHECK THAT L.R RAISES CONSTRAINT_ERROR WHEN " &
                      "THE COMPONENT DENOTED BY R DOES NOT EXIST");

     DECLARE

          VR : V := (DISC => 0, Y => 4);
          J : INTEGER;

     BEGIN

          IF EQUAL (4, 4) THEN
               VR := (DISC => 1, X => 3);
          END IF;

          J := VR.Y;
          FAILED ("CONSTRAINT_ERROR NOT RAISED FOR A RECORD OBJECT");

          -- IF STATEMENT PREVENTS OPTIMIZING OF VARIABLE J.

          IF EQUAL (J,3) THEN
               FAILED ("CONSTRAINT_ERROR NOT RAISED - 1");
          END IF;

     EXCEPTION

          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR A RECORD OBJECT");

     END;

     --------------------------------------------------

     DECLARE

          J : INTEGER;

          FUNCTION F RETURN V IS
          BEGIN
               IF EQUAL (4, 4) THEN
                    RETURN (DISC => 2, Y => 3);
               END IF;
               RETURN (DISC => 1, X => 4);
          END F;

     BEGIN

          J := F.X;
          FAILED ("CONSTRAINT_ERROR NOT RAISED FOR A FUNCTION CALL " &
                  "DELIVERING A RECORD VALUE");

          -- IF STATEMENT PREVENTS OPTIMIZING OF VARIABLE J.

          IF EQUAL (J,3) THEN
               FAILED ("CONSTRAINT_ERROR NOT RAISED - 2");
          END IF;

     EXCEPTION

          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR A FUNCTION CALL " &
                       "DELIVERING A RECORD VALUE");

     END;

     --------------------------------------------------

     DECLARE

          A : T := NEW V' (DISC => 0, Y => 4);
          J : INTEGER;

     BEGIN

          IF EQUAL (4, 4) THEN
               A := NEW V' (DISC => 1, X => 3);
          END IF;

          J := A.Y;
          FAILED ("CONSTRAINT_ERROR NOT RAISED FOR AN ACCESS OBJECT");

          -- IF STATEMENT PREVENTS OPTIMIZING OF VARIABLE J.

          IF EQUAL (J,3) THEN
               FAILED ("CONSTRAINT_ERROR NOT RAISED - 3");
          END IF;

     EXCEPTION

          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR AN ACCESS OBJECT");

     END;

     --------------------------------------------------

     DECLARE

          J : INTEGER;

          FUNCTION F RETURN T IS
          BEGIN
               IF EQUAL (4, 4) THEN
                    RETURN NEW V' (DISC => 2, Y => 3);
               END IF;
               RETURN NEW V' (DISC => 1, X => 4);
          END F;

     BEGIN

          J := F.X;
          FAILED ("CONSTRAINT_ERROR NOT RAISED FOR A FUNCTION CALL " &
                  "DELIVERING AN ACCESS VALUE");

          -- IF STATEMENT PREVENTS OPTIMIZING OF VARIABLE J.

          IF EQUAL (J,3) THEN
               FAILED ("CONSTRAINT_ERROR NOT RAISED - 4");
          END IF;

     EXCEPTION

          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR A FUNCTION CALL " &
                       "DELIVERING AN ACCESS VALUE");

     END;

     RESULT;
END C41304B;
