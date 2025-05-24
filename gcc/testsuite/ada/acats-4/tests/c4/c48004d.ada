-- C48004D.ADA

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
-- CHECK THAT THE FORM "NEW T" IS PERMITTED IF T IS A RECORD, PRIVATE,
-- OR LIMITED TYPE WITHOUT DISCRIMINANTS.

-- RM  01/12/80
-- JBG 03/03/83
-- EG  07/05/84

WITH REPORT;

PROCEDURE C48004D IS

     USE REPORT;

BEGIN

     TEST("C48004D","CHECK THAT THE FORM 'NEW T' IS PERMITTED IF T " &
                    "IS A RECORD, PRIVATE, OR LIMITED TYPE WITHOUT " &
                    "DISCRIMINANTS");

     DECLARE

          TYPE  TC  IS
               RECORD
                    C : INTEGER := 18;
               END RECORD;
          TYPE ATC IS ACCESS TC;
          VC : ATC;

          PACKAGE  P  IS
               TYPE   PRIV  IS PRIVATE;
               TYPE  LPRIV  IS LIMITED PRIVATE;
               TYPE A_PRIV  IS  ACCESS PRIV;
               TYPE A_LPRIV  IS  ACCESS LPRIV;
               PROCEDURE   CHECK( X: A_PRIV  );
               PROCEDURE  LCHECK( X: A_LPRIV );
               PROCEDURE LRCHECK( X: LPRIV );
          PRIVATE
               TYPE   PRIV  IS
                    RECORD
                         Q : INTEGER := 19;
                    END RECORD;
               TYPE  LPRIV  IS
                    RECORD
                         Q : INTEGER := 20;
                    END RECORD;
          END P;

     
          VP  :  P.A_PRIV;
          VLP : P.A_LPRIV;

          TYPE LCR IS
               RECORD
                    C : P.LPRIV;
               END RECORD;
          TYPE A_LCR IS ACCESS LCR;
          VLCR : A_LCR;

          PACKAGE BODY  P  IS

               PROCEDURE  CHECK( X: A_PRIV )  IS
               BEGIN
                    IF  X.Q /= 19  THEN  FAILED( "WRONG VALUES - C2" );
                    END IF;
               END CHECK;
     
               PROCEDURE  LCHECK( X: A_LPRIV )  IS
               BEGIN
                    IF  X.Q /= 20  THEN  FAILED( "WRONG VALUES - C3" );
                    END IF;
               END LCHECK;

               PROCEDURE LRCHECK (X : LPRIV) IS
               BEGIN
                    IF X.Q /= 20 THEN
                         FAILED ("WRONG VALUES - C4");
                    END IF;
               END LRCHECK;

          END P;

     BEGIN

          VC  :=  NEW TC;
          IF  VC.C /= 18  THEN FAILED( "WRONG VALUES  -  C1" );
          END IF;
          
          VP  :=  NEW P.PRIV;
          P.CHECK( VP );
          VLP :=  NEW P.LPRIV;
          P.LCHECK( VLP );

          VLCR := NEW LCR;
          P.LRCHECK( VLCR.ALL.C );

     END;

     RESULT;

END C48004D;
