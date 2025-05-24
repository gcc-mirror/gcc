-- C48006A.ADA

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
-- CHECK THAT AN ALLOCATOR OF THE FORM "NEW T'(X)" ALLOCATES A NEW 
-- OBJECT EACH TIME IT IS EXECUTED AND THAT IF T IS A SCALAR OR ACCESS
-- TYPE, THE ALLOCATED OBJECT HAS THE VALUE OF X.

-- RM  01/14/80
-- RM  01/O1/82
-- SPS 10/27/82
-- EG  07/05/84

WITH REPORT;

PROCEDURE C48006A IS

     USE REPORT;

BEGIN

     TEST("C48006A","CHECK THAT THE FORM 'NEW T'(X)' " &
                    "ALLOCATES A NEW OBJECT " &
                    "AND THAT IF T IS A SCALAR OR ACCESS TYPE, THE "   &
                    "ALLOCATED OBJECT HAS THE VALUE OF X");

     DECLARE

          TYPE ATA IS ACCESS INTEGER;
          TYPE AATA IS ACCESS ATA;
          VA1, VA2, VA3 : ATA;
          VAA1, VAA2, VAA3 : AATA;

     BEGIN

          VA1 := NEW INTEGER'(5 + 7);
          IF VA1.ALL /= IDENT_INT(12) THEN
               FAILED("WRONG VALUES - VA1");
          END IF;

          VA2 := NEW INTEGER'(1 + 2);
          IF (VA1.ALL /= IDENT_INT(12) OR
              VA2.ALL /= IDENT_INT( 3)) THEN
               FAILED("WRONG VALUES - VA2");
          END IF;

          VA3 := NEW INTEGER'(IDENT_INT(3) + IDENT_INT(4));
          IF (VA1.ALL /= IDENT_INT(12) OR
              VA2.ALL /= IDENT_INT( 3) OR
              VA3.ALL /= IDENT_INT( 7)) THEN
               FAILED("WRONG VALUES - VA3");
          END IF;

          VAA1 := NEW ATA'(NEW INTEGER'(3));
          IF VAA1.ALL.ALL /= IDENT_INT(3) THEN
               FAILED ("WRONG VALUES - VAA1");
          END IF;

          VAA2 := NEW ATA'(NEW INTEGER'(IDENT_INT(5)));
          IF (VAA1.ALL.ALL /= 3 OR
              VAA2.ALL.ALL /= 5 ) THEN
               FAILED ("WRONG VALUES - VAA2");
          END IF;

          VAA3 := NEW ATA'(NEW INTEGER'(IDENT_INT(6)));
          IF (VAA1.ALL.ALL /= 3 OR
              VAA2.ALL.ALL /= 5 OR
              VAA3.ALL.ALL /= 6 ) THEN
               FAILED ("WRONG VALUES - VAA3");
          END IF;

     END;

     RESULT;

END C48006A;
