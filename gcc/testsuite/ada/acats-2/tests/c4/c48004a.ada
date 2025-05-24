-- C48004A.ADA

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
-- CHECK THAT THE FORM "NEW T" IS PERMITTED IF T IS A SCALAR SUBTYPE.

-- RM  01/12/80
-- JBG 03/03/83
-- EG  07/05/84

WITH REPORT;

PROCEDURE  C48004A  IS

     USE REPORT;

BEGIN

     TEST("C48004A","CHECK THAT THE FORM 'NEW T' IS PERMITTED IF " &
                    "T IS A SCALAR SUBTYPE");

     DECLARE

          SUBTYPE TA IS INTEGER RANGE 1 .. 7;
          TYPE ATA IS ACCESS TA;
          VA : ATA;

     BEGIN

          VA := NEW TA;
          VA.ALL := IDENT_INT(6);
          IF VA.ALL /= 6 THEN
               FAILED ("INCORRECT VALUE");
          END IF;

     END;

     RESULT;

END C48004A;
