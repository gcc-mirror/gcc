-- CE2201D.DEP

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
--     CHECK WHETHER READ, WRITE, AND END_OF_FILE ARE SUPPORTED FOR
--     SEQUENTIAL FILES WITH ELEMENT_TYPE UNCONSTRAINED ARRAY.

--     IF I/O IS NOT SUPPORTED, THEN CREATE AND OPEN CAN RAISE USE_ERROR
--     OR NAME_ERROR. SEE (AI-00332).

-- APPLICABILITY CRITERIA:
--     THIS TEST IS NON-APPLICABLE IF THE INSTANTIATION OF SEQUENTIAL_IO
--     WITH UNCONSTRAINED ARRAY TYPE, ARR_UNCN, IS NOT SUPPORTED.

--     IF THE INSTANTIATION OF SEQUENTIAL_IO IS NOT SUPPORTED THEN
--     THE INSTANTIATION MUST BE REJECTED.

-- HISTORY:
--     ABW  8/17/82
--     SPS 9/15/82
--     SPS 11/9/82
--     JBG 1/6/83
--     JBG 6/4/84
--     TBN 11/01/85     RENAMED FROM CE2201D.DEP AND MODIFIED COMMENTS.
--     TBN 11/04/86     REVISED TEST TO OUTPUT A NON_APPLICABLE
--                      RESULT WHEN FILES ARE NOT SUPPORTED.
--     THS 03/30/90     RENAMED FROM EE2201D.ADA.

WITH REPORT; USE REPORT;
WITH SEQUENTIAL_IO;

PROCEDURE CE2201D IS
     INCOMPLETE : EXCEPTION;
BEGIN

     TEST ("CE2201D" , "CHECK WHETHER READ, WRITE, AND END_OF_FILE " &
                       "ARE SUPPORTED FOR SEQUENTIAL FILES WITH " &
                       "UNCONSTRAINED ARRAY TYPES");

     DECLARE
          SUBTYPE ONE_TEN IS INTEGER RANGE 1..10;
          TYPE ARR_UNCN IS ARRAY (ONE_TEN RANGE <>) OF INTEGER;
          PACKAGE SEQ_ARR_UNCN
               IS NEW SEQUENTIAL_IO (ARR_UNCN); -- N/A => ERROR.
          USE SEQ_ARR_UNCN;
          FILE_ARR_UNCN : FILE_TYPE;
          ARR2 : ARR_UNCN (1..6) := (1,3,5,7,9,0);
          ITEM_ARR2 : ARR_UNCN (1..6);
     BEGIN
          BEGIN
               CREATE (FILE_ARR_UNCN);

          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; SEQUENTIAL " &
                                    "CREATE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED; SEQUENTIAL " &
                                    "CREATE");
                    RAISE INCOMPLETE;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED; SEQUENTIAL " &
                            "CREATE");
                    RAISE INCOMPLETE;
          END;

          BEGIN
               WRITE (FILE_ARR_UNCN,ARR2);
               WRITE (FILE_ARR_UNCN, (0, -2));

          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("WRITE FOR UNCONSTRAINED ARRAY");
          END;

          RESET (FILE_ARR_UNCN,IN_FILE);

          IF END_OF_FILE (FILE_ARR_UNCN) THEN
               FAILED ("WRONG END_OF_FILE VALUE FOR " &
                       "UNCONSTRAINED ARRAY");
          END IF;

          BEGIN
               READ (FILE_ARR_UNCN,ITEM_ARR2);

          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("READ FOR UNCONSTRAINED ARRAY");
          END;

          IF ITEM_ARR2 /= (1,3,5,7,9,0) THEN
               FAILED ("READ WRONG VALUE - 1");
          END IF;

          BEGIN
               READ (FILE_ARR_UNCN, ITEM_ARR2(3..4));

               IF ITEM_ARR2 /= (1,3,0,-2,9,0) THEN
                    FAILED ("READ WRONG VALUE - 2");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION FOR SECOND ARRAY READ");
          END;

          IF NOT END_OF_FILE(FILE_ARR_UNCN) THEN
               FAILED ("NOT AT END OF FILE");
          END IF;

          CLOSE (FILE_ARR_UNCN);

     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED BY RESET");
     END;

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE2201D;
