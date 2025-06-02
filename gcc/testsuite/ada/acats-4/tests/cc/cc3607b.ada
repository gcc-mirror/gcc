-- CC3607B.ADA

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
--     CHECK THAT WHEN A DEFAULT SUBPROGRAM IS SPECIFIED WITH A BOX, A
--     SUBPROGRAM DIRECTLY VISIBLE AT THE POINT OF INSTANTIATION
--     IS USED.

-- HISTORY:
--     LDC 08/23/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE CC3607B IS

BEGIN
     TEST ("CC3607B", "CHECK THAT WHEN A DEFAULT SUBPROGRAM IS " &
                      "SPECIFIED WITH A BOX, A SUBPROGRAM DIRECTLY " &
                      "VISIBLE AT THE POINT OF INSTANTIATION IS USED");
     DECLARE
          PACKAGE PROC_PACK IS
               PROCEDURE PROC;

               GENERIC
                    WITH PROCEDURE PROC IS <>;
               PACKAGE GEN_PACK IS
                    PROCEDURE DO_PROC;
               END GEN_PACK;
          END PROC_PACK;
          USE PROC_PACK;

          PACKAGE BODY PROC_PACK IS
               PROCEDURE PROC IS
               BEGIN
                    FAILED("WRONG SUBPROGRAM WAS USED");
               END PROC;

               PACKAGE BODY GEN_PACK IS
                    PROCEDURE DO_PROC IS
                    BEGIN
                         PROC;
                    END DO_PROC;
               END GEN_PACK;
          END PROC_PACK;

          PROCEDURE PROC IS
          BEGIN
               COMMENT ("SUBPROGRAM VISIBLE AT INSTANTIATION WAS " &
                        "USED");
          END PROC;

          PACKAGE NEW_PACK IS NEW GEN_PACK;

     BEGIN
          NEW_PACK.DO_PROC;
     END;

     RESULT;
END CC3607B;
