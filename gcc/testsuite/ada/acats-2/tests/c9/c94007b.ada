-- C94007B.ADA

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
-- CHECK THAT A TASK THAT IS ALLOCATED IN A NON-LIBRARY PACKAGE
--   (SPECIFICATION OR BODY) DOES NOT "DEPEND" ON THE PACKAGE,
--   BUT ON THE INNERMOST ENCLOSING BLOCK, SUBPROGRAM BODY,
--   OR TASK BODY.
-- SUBTESTS ARE:
--   (A)  A SIMPLE TASK ALLOCATOR, IN A VISIBLE PART, IN A BLOCK.
--   (B)  A RECORD OF TASK ALLOCATOR, IN A PRIVATE PART, IN A FUNCTION.
--   (C)  A RECORD OF ARRAY OF TASK ALLOCATOR, IN A PACKAGE BODY,
--           IN A TASK BODY.

-- JRK 10/16/81
-- SPS 11/2/82
-- PWN 01/31/95  REMOVED PRAGMA PRIORITY FOR ADA 9X.

with Impdef;
WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE C94007B IS

     TASK TYPE SYNC IS
          ENTRY ID (C : CHARACTER);
          ENTRY INNER;
          ENTRY OUTER;
     END SYNC;

     TASK BODY SYNC IS
          ID_C : CHARACTER;
     BEGIN
          ACCEPT ID (C : CHARACTER) DO
               ID_C := C;
          END ID;
          DELAY 1.0 * Impdef.One_Second;
          SELECT
               ACCEPT OUTER;
          OR
               DELAY 120.0 * Impdef.One_Second;
               FAILED ("PROBABLY BLOCKED - (" & ID_C & ')');
          END SELECT;
          ACCEPT INNER;
     END SYNC;


BEGIN
     TEST ("C94007B", "CHECK THAT A TASK THAT IS ALLOCATED IN A " &
                      "NON-LIBRARY PACKAGE (SPECIFICATION OR BODY) " &
                      "DOES NOT ""DEPEND"" ON THE PACKAGE, BUT ON " &
                      "THE INNERMOST ENCLOSING BLOCK, SUBPROGRAM " &
                      "BODY, OR TASK BODY");

     --------------------------------------------------

     DECLARE -- (A)

          S : SYNC;

     BEGIN -- (A)

          S.ID ('A');

          DECLARE

               PACKAGE PKG IS
                    TASK TYPE TT IS
                         ENTRY E;
                    END TT;
                    TYPE A_T IS ACCESS TT;
                    A : A_T;
               END PKG;

               PACKAGE BODY PKG IS
                    TASK BODY TT IS
                    BEGIN
                         S.INNER;  -- PROBABLE INNER BLOCK POINT.
                    END TT;
               BEGIN
                    A := NEW TT;
               END PKG;            -- PROBABLE OUTER BLOCK POINT.

          BEGIN

               S.OUTER;

          EXCEPTION
               WHEN TASKING_ERROR => NULL;
          END;

     END; -- (A)

     --------------------------------------------------

     DECLARE -- (B)

          S : SYNC;

          I : INTEGER;

          FUNCTION F RETURN INTEGER IS

               PACKAGE PKG IS
               PRIVATE
                    TASK TYPE TT IS
                         ENTRY E;
                    END TT;

                    TYPE RT IS
                         RECORD
                              T : TT;
                         END RECORD;

                    TYPE ART IS ACCESS RT;

                    AR : ART;
               END PKG;

               PACKAGE BODY PKG IS
                    TASK BODY TT IS
                    BEGIN
                         S.INNER;  -- PROBABLE INNER BLOCK POINT.
                    END TT;
               BEGIN
                    AR := NEW RT;
               END PKG;            -- PROBABLE OUTER BLOCK POINT.

          BEGIN -- F

               S.OUTER;
               RETURN 0;

          EXCEPTION
               WHEN TASKING_ERROR => RETURN 0;
          END F;

     BEGIN -- (B)

          S.ID ('B');
          I := F ;

     END; -- (B)

     --------------------------------------------------

     DECLARE -- (C)

          S : SYNC;

     BEGIN -- (C)

          S.ID ('C');

          DECLARE

               TASK TSK IS
               END TSK;

               TASK BODY TSK IS

                    PACKAGE PKG IS
                    END PKG;

                    PACKAGE BODY PKG IS
                         TASK TYPE TT IS
                              ENTRY E;
                         END TT;
                         
                         TYPE ARR IS ARRAY (1..1) OF TT;
                         TYPE RAT IS
                              RECORD
                                   T : ARR;
                              END RECORD;

                         TYPE ARAT IS ACCESS RAT;

                         ARA : ARAT;

                         TASK BODY TT IS
                         BEGIN
                              S.INNER;  -- PROBABLE INNER BLOCK POINT.
                         END TT;
                    BEGIN
                         ARA := NEW RAT;
                    END PKG;            -- PROBABLE OUTER BLOCK POINT.

               BEGIN -- TSK

                    S.OUTER;

               EXCEPTION
                    WHEN TASKING_ERROR => NULL;
               END TSK;

          BEGIN
               NULL;
          END;

     END; -- (C)

     --------------------------------------------------

     RESULT;
END C94007B;
