-- C48004B.ADA

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
-- CHECK THAT THE FORM "NEW T" IS PERMITTED IF T IS A CONSTRAINED
-- RECORD, PRIVATE, OR LIMITED PRIVATE TYPE.

-- RM  01/12/80
-- JBG 03/03/83
-- EG  07/05/84

WITH REPORT;

PROCEDURE C48004B IS

     USE REPORT;

BEGIN

     TEST("C48004B","CHECK THAT THE FORM 'NEW T' IS PERMITTED IF " &
                    "T IS A CONSTRAINED RECORD, PRIVATE, OR "      &
                    "LIMITED PRIVATE TYPE");

     DECLARE

          TYPE  TB0(A , B : INTEGER )  IS
               RECORD
                    C : INTEGER := 7;
               END RECORD;
          SUBTYPE  TB  IS  TB0( 2 , 3 );
          TYPE ATB IS ACCESS TB0;
          VB : ATB;

          TYPE  TBB0(  A , B : INTEGER := 5 )  IS
               RECORD
                    C : INTEGER := 6;
               END RECORD;
          SUBTYPE  TBB  IS  TBB0( 4 , 5 );
          TYPE ATBB IS ACCESS TBB0;
          VBB : ATBB;

          PACKAGE  P  IS
               TYPE   PRIV0( A , B : INTEGER )  IS PRIVATE;
               TYPE  LPRIV0( A , B : INTEGER := 1 )  IS LIMITED PRIVATE;
               FUNCTION FUN(LP : LPRIV0) RETURN INTEGER;
          PRIVATE
               TYPE  PRIV0( A , B : INTEGER )  IS
                    RECORD
                         Q : INTEGER;
                    END RECORD;
               TYPE  LPRIV0( A , B : INTEGER := 1 )  IS
                    RECORD
                         Q : INTEGER := 7;
                    END RECORD;
          END P;
     
          USE P;

          SUBTYPE  PRIV  IS  P.PRIV0( 12 , 13 );
          TYPE  A_PRIV  IS  ACCESS P.PRIV0;
          VP : A_PRIV;

          TYPE  A_LPRIV  IS  ACCESS LPRIV0;
          VLP : A_LPRIV;

          TYPE LCR(A, B : INTEGER := 4) IS
               RECORD
                    C : P.LPRIV0;
               END RECORD;
          SUBTYPE SLCR IS LCR(1, 2);
          TYPE A_SLCR IS ACCESS SLCR;
          VSLCR : A_SLCR;

          PACKAGE BODY P IS
               FUNCTION FUN(LP : LPRIV0) RETURN INTEGER IS
               BEGIN
                    RETURN LP.Q;
               END FUN;
          END P;

     BEGIN

          VB  :=  NEW TB;
          IF ( VB.A /= IDENT_INT(2)  OR
               VB.B /= 3  OR
               VB.C /= 7 )  THEN FAILED( "WRONG VALUES  -  B1" );
          END IF;

          VBB  :=  NEW TBB0;
          IF ( VBB.A /= IDENT_INT(5)  OR
               VBB.B /= 5  OR
               VBB.C /= 6 )  THEN
               FAILED( "WRONG VALUES  -  B2" );
          END IF;
          
          VP  :=  NEW PRIV;
          IF ( VP.A /= IDENT_INT(12)  OR
               VP.B /= 13 )  THEN 
               FAILED( "WRONG VALUES  -  B3" );
          END IF;
          
          VLP  :=  NEW LPRIV0;
          IF ( VLP.A /= IDENT_INT(1)  OR
               VLP.B /= 1 OR
               P.FUN(VLP.ALL) /= IDENT_INT(7) )  THEN 
               FAILED( "WRONG VALUES  -  B4" );
          END IF;

          VSLCR := NEW SLCR;
          IF ( VSLCR.A /= IDENT_INT(1) OR
               VSLCR.B /= IDENT_INT(2) OR
               P.FUN(VSLCR.C) /= IDENT_INT(7) ) THEN
               FAILED ("WRONG VALUES - B5");
          END IF;

     END;

     RESULT;

END C48004B;
