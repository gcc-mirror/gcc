-- C83027C.ADA

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
--     CHECK THAT A DECLARATION WITHIN THE DISCRIMINANT PART OF A
--     PRIVATE TYPE DECLARATION, AN INCOMPLETE TYPE DECLARATION, AND A
--     GENERIC FORMAL TYPE DECLARATION HIDES AN OUTER DECLARATION OF A
--     HOMOGRAPH.  ALSO, CHECK THAT THE OUTER DECLARATION IS DIRECTLY
--     VISIBLE IN BOTH DECLARATIVE REGIONS BEFORE THE DECLARATION OF THE
--     INNER HOMOGRAPH AND THE OUTER DECLARATION IS VISIBLE BY SELECTION
--     AFTER THE INNER HOMOGRAPH DECLARATION.

-- HISTORY:
--     BCB 09/06/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C83027C IS

     GENERIC
          TYPE T IS PRIVATE;
          X : T;
     FUNCTION GEN_FUN RETURN T;

     FUNCTION GEN_FUN RETURN T IS
     BEGIN
          RETURN X;
     END GEN_FUN;

BEGIN
     TEST ("C83027C", "CHECK THAT A DECLARATION IN THE DISCRIMINANT " &
                      "PART OF A PRIVATE TYPE DECLARATION, AN " &
                      "INCOMPLETE TYPE DECLARATION, AND A GENERIC " &
                      "FORMAL TYPE DECLARATION HIDES AN OUTER " &
                      "DECLARATION OF A HOMOGRAPH");

     ONE:
     DECLARE
          A : INTEGER := IDENT_INT(2);

          D : INTEGER := IDENT_INT(2);

          G : INTEGER := IDENT_INT(2);
          H : INTEGER := G;

          TYPE REC (Z : INTEGER) IS RECORD
               NULL;
          END RECORD;

          GENERIC
               TYPE INNER3 (G : INTEGER) IS PRIVATE;
          PACKAGE P_ONE IS
               TYPE INNER (X : INTEGER := A;
                           A : INTEGER := IDENT_INT(3);
                           C : INTEGER := ONE.A) IS PRIVATE;
               TYPE INNER2 (Y : INTEGER := D;
                            D : INTEGER := IDENT_INT(3);
                            F : INTEGER := ONE.D);
               TYPE INNER2 (Y : INTEGER := D;
                            D : INTEGER := IDENT_INT(3);
                            F : INTEGER := ONE.D) IS RECORD
                    E : INTEGER := D;
               END RECORD;
          PRIVATE
               TYPE INNER (X : INTEGER := A;
                           A : INTEGER := IDENT_INT(3);
                           C : INTEGER := ONE.A) IS RECORD
                    B : INTEGER := A;
               END RECORD;
          END P_ONE;

          PACKAGE BODY P_ONE IS
               RECVAR : INNER;
               RECVAR2 : INNER2;
               RECVAR3 : INNER3(3);
          BEGIN
               IF RECVAR.A /= IDENT_INT(3) THEN
                    FAILED ("INCORRECT VALUE FOR INNER HOMOGRAPH - 1");
               END IF;

               IF A /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 2");
               END IF;

               IF RECVAR.B /= IDENT_INT(3) THEN
                    FAILED ("INCORRECT VALUE FOR INNER VARIABLE - 3");
               END IF;

               IF RECVAR.C /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR INNER VARIABLE - 4");
               END IF;

               IF RECVAR.X /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR INNER VARIABLE - 5");
               END IF;

               IF RECVAR2.D /= IDENT_INT(3) THEN
                    FAILED ("INCORRECT VALUE FOR INNER HOMOGRAPH - 6");
               END IF;

               IF D /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 7");
               END IF;

               IF RECVAR2.E /= IDENT_INT(3) THEN
                    FAILED ("INCORRECT VALUE FOR INNER VARIABLE - 8");
               END IF;

               IF RECVAR2.F /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR INNER VARIABLE - 9");
               END IF;

               IF RECVAR2.Y /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR INNER VARIABLE - 10");
               END IF;

               IF RECVAR3.G /= IDENT_INT(3) THEN
                    FAILED ("INCORRECT VALUE FOR INNER HOMOGRAPH - 11");
               END IF;

               IF G /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 12");
               END IF;

               IF H /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR OUTER VARIABLE - 13");
               END IF;
          END P_ONE;

          PACKAGE NEW_P_ONE IS NEW P_ONE (REC);

     BEGIN  -- ONE
          NULL;
     END ONE;

     RESULT;
END C83027C;
