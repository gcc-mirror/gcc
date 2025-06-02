-- CC1310A.ADA

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
-- CHECK THAT DEFAULT GENERIC SUBPROGRAM PARAMETERS MAY BE ENTRIES.

-- DAT 9/8/81
-- SPS 2/7/83

WITH REPORT; USE REPORT;

PROCEDURE CC1310A IS
BEGIN
     TEST ("CC1310A", "DEFAULT GENERIC SUBPROGRAM PARAMETERS MAY BE"
          & " ENTRIES");

     DECLARE
          TASK T IS
               ENTRY ENT1;
               ENTRY ENT2 (I : IN INTEGER);
          END T;

          PROCEDURE P1 RENAMES T.ENT1;
          
          PROCEDURE P4 (I : IN INTEGER) RENAMES T.ENT2;

          INT : INTEGER := 0;

          TASK BODY T IS
          BEGIN
               ACCEPT ENT1;
               ACCEPT ENT2 (I : IN INTEGER) DO
                    INT := INT + I;
               END ENT2;
               ACCEPT ENT2 (I : IN INTEGER) DO
                    INT := INT + I;
               END ENT2;
               ACCEPT ENT1;
          END T;

     BEGIN
          DECLARE
               GENERIC 
                    WITH PROCEDURE P1 IS <> ;
                    WITH PROCEDURE P2 IS T.ENT1;
                    WITH PROCEDURE P3 (I : IN INTEGER) IS T.ENT2;
                    WITH PROCEDURE P4 (I : IN INTEGER) IS <> ;
               PACKAGE PKG IS END PKG;

               PACKAGE BODY PKG IS
               BEGIN
                    P1;
                    P4 (3);
                    P3 (6);
                    P2;
               END PKG;

               PACKAGE PP IS NEW PKG;

          BEGIN
               IF INT /= 9 THEN
                    FAILED ("ENTRIES AS DEFAULT GENERIC PARAMETERS");
               END IF;
          END;
     END;

     RESULT;
END CC1310A;
