-- C41402A.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS NOT RAISED IF THE PREFIX OF
--     'ADDRESS, 'SIZE, 'FIRST_BIT, 'LAST_BIT, AND 'POSITION HAS THE
--     VALUE NULL.

-- HISTORY:
--     TBN  10/02/86  CREATED ORIGINAL TEST.
--     CJJ  07/01/87  REMOVED TEST FOR 'STORAGE_SIZE, WHICH IS NO LONGER
--                    PART OF THE OBJECTIVE.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;
PROCEDURE C41402A IS

     TYPE ARRAY1 IS ARRAY (1 .. 2) OF INTEGER;
     TYPE ACC_ARA IS ACCESS ARRAY1;

     PTR_ARA : ACC_ARA;
     VAR1 : INTEGER;

     TYPE REC1 IS
          RECORD
               A : INTEGER;
          END RECORD;

     TYPE ACC_REC1 IS ACCESS REC1;

     TYPE REC2 IS
          RECORD
               P_AR : ACC_ARA;
               P_REC : ACC_REC1;
          END RECORD;

     OBJ_REC : REC2;


     PROCEDURE PROC (A : ADDRESS) IS
     BEGIN
          NULL;
     END;

BEGIN
     TEST ("C41402A", "CHECK THAT CONSTRAINT_ERROR IS NOT RAISED IF " &
                      "THE PREFIX OF 'ADDRESS, 'SIZE, " &
                      "'FIRST_BIT, 'LAST_BIT, AND 'POSITION HAS THE " &
                      "VALUE NULL");

     BEGIN
          PROC (PTR_ARA'ADDRESS);
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CONSTRAINT_ERROR RAISED FOR 'ADDRESS");
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED 'ADDRESS");
     END;

     BEGIN
          VAR1 := PTR_ARA'SIZE;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CONSTRAINT_ERROR RAISED FOR 'SIZE");
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED 'SIZE");
     END;

     BEGIN
          VAR1 := OBJ_REC.P_AR'FIRST_BIT;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CONSTRAINT_ERROR RAISED FOR 'FIRST_BIT");
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED 'FIRST_BIT");
     END;

     BEGIN
          VAR1 := OBJ_REC.P_AR'LAST_BIT;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CONSTRAINT_ERROR RAISED FOR 'LAST_BIT");
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED 'LAST_BIT");
     END;

     BEGIN
          VAR1 := OBJ_REC.P_REC'POSITION;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CONSTRAINT_ERROR RAISED FOR 'POSITION");
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED 'POSITION");
     END;

     RESULT;
END C41402A;
