-- C38005B.ADA

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
--     CHECK THAT ANY OBJECT WITH A FORMAL PRIVATE TYPE, WHOSE ACTUAL
--     TYPE IN AN INSTANTIATION IS AN ACCESS TYPE, IS INITIALIZED BY
--     DEFAULT TO THE VALUE NULL. THIS INCLUDES OBJECTS WHICH ARE ARRAY
--     AND RECORD COMPONENTS.

-- HISTORY:
--     DHH 07/12/88 CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C38005B IS

BEGIN
     TEST("C38005B", "CHECK THAT ANY OBJECT WITH A FORMAL PRIVATE " &
                     "TYPE, WHOSE ACTUAL TYPE IN AN INSTANTIATION " &
                     "IS AN ACCESS TYPE, IS INITIALIZED BY DEFAULT " &
                     "TO THE VALUE NULL. THIS INCLUDES OBJECTS WHICH " &
                     "ARE ARRAY AND RECORD COMPONENTS");
     DECLARE
          TYPE ARRY IS ARRAY(1 .. 10) OF BOOLEAN;
          TYPE REC1 IS
               RECORD
                    A : INTEGER;
                    B : ARRY;
               END RECORD;

          TYPE POINTER IS ACCESS REC1;

          GENERIC
               TYPE NEW_PTR IS PRIVATE;
          PACKAGE GEN_PACK IS
               TYPE PTR_ARY IS ARRAY(1 .. 5) OF NEW_PTR;
               TYPE RECORD1 IS
                    RECORD
                         A : NEW_PTR;
                         B : PTR_ARY;
                    END RECORD;

               OBJ : NEW_PTR;
               ARY : PTR_ARY;
               REC : RECORD1;
          END GEN_PACK;

          PACKAGE TEST_P IS NEW GEN_PACK(POINTER);
          USE TEST_P;

     BEGIN
          IF OBJ /= NULL THEN
               FAILED("OBJECT NOT INITIALIZED TO NULL");
          END IF;

          FOR I IN 1 .. 5 LOOP
               IF ARY(I) /= NULL THEN
                    FAILED("ARRAY COMPONENT " &
                            INTEGER'IMAGE(I) &
                           " NOT INITIALIZED TO NULL");
               END IF;
          END LOOP;

          IF REC.A /= NULL THEN
               FAILED("RECORD OBJECT NOT INITIALIZED TO NULL");
          END IF;

          FOR I IN 1 .. 5 LOOP
               IF REC.B(I) /= NULL THEN
                    FAILED("RECORD SUBCOMPONENT " &
                           INTEGER'IMAGE(I) &
                           " NOT INITIALIZED TO NULL");
               END IF;
          END LOOP;
     END;

     RESULT;
END C38005B;
