--  C45611A.ADA

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
--  CHECK THAT EXPONENTIATION OF AN INTEGER TO AN INTEGER VALUE IS 
--  CORRECTLY EVALUATED.

--  H. TILTON 9/23/86

WITH REPORT; USE REPORT;

PROCEDURE C45611A IS

    I1,INT : INTEGER;
   
    BEGIN

 
         TEST ("C45611A", "CHECK THAT EXPONENTIATION OF AN INTEGER " &
                          "VALUE IS CORRECTLY EVALUATED");

         I1 := IDENT_INT(0) ** IDENT_INT(0);

         IF IDENT_INT(I1) /= IDENT_INT(1) THEN
              FAILED( "INCORRECT RESULT FOR '0**0'" ); 
         END IF;

         INT := "**" (IDENT_INT(0),IDENT_INT(1));

         IF IDENT_INT(INT) /= IDENT_INT(0) THEN
              FAILED( "INCORRECT RESULT FOR '0**1'" ); 
         END IF;

         I1 := IDENT_INT(6) ** IDENT_INT(0);

         IF IDENT_INT(I1) /= IDENT_INT(1) THEN
              FAILED( "INCORRECT RESULT FOR '6**0'" ); 
         END IF;

         INT := IDENT_INT(156) ** IDENT_INT(1);

         IF IDENT_INT(INT) /= IDENT_INT(156) THEN
              FAILED( "INCORRECT RESULT FOR '156**1'" ); 
         END IF;

         I1 := IDENT_INT(-3) ** IDENT_INT(0);

         IF IDENT_INT(I1) /= IDENT_INT(1) THEN
              FAILED( "INCORRECT RESULT FOR '(-3)**0'" ); 
         END IF;

         INT := "**" (IDENT_INT(-7),IDENT_INT(1));

         IF IDENT_INT(INT) /= IDENT_INT(-7) THEN
              FAILED( "INCORRECT RESULT FOR '(-7)**1'" ); 
         END IF;

         I1 := "**" (IDENT_INT(-1),IDENT_INT(2));

         IF IDENT_INT(I1) /= IDENT_INT(1) THEN
              FAILED( "INCORRECT RESULT FOR '(-1)**2'" );
         END IF;
  
  
         INT := IDENT_INT(-1) ** 3;         

         IF IDENT_INT(INT) /= IDENT_INT(-1) THEN
              FAILED( "INCORRECT RESULT FOR '(-1)**3'" );
         END IF;
  
         INT := "**" (IDENT_INT(0),IDENT_INT(2));

         IF IDENT_INT(INT) /= IDENT_INT(0) THEN
              FAILED( "INCORRECT RESULT FOR '0**2'" );
         END IF;

         INT := IDENT_INT(0) ** IDENT_INT(10);

         IF IDENT_INT(INT) /= IDENT_INT(0) THEN
              FAILED( "INCORRECT RESULT FOR '0**10'" );
         END IF;
  
         INT := "**" (IDENT_INT(6),IDENT_INT(2));

         IF IDENT_INT(INT) /= IDENT_INT(36) THEN
              FAILED( "INCORRECT RESULT FOR '6**2'" );
         END IF;
  
         INT := "**" (IDENT_INT(2),IDENT_INT(2));

         IF IDENT_INT(INT) /= IDENT_INT(4) THEN
              FAILED( "INCORRECT RESULT FOR '2**2'" );
         END IF;

         I1 := "**" (IDENT_INT(1),IDENT_INT(10));

         IF IDENT_INT(I1) /= IDENT_INT(1) THEN
              FAILED( "INCORRECT RESULT FOR '1**10'" );
         END IF;
        
         RESULT;

    END C45611A;      
