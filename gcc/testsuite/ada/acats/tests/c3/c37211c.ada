-- C37211C.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED BY A DISCRIMINANT CONSTRAINT 
-- IF A VALUE SPECIFIED FOR A DISCRIMINANT DOES NOT LIE IN THE RANGE
-- OF THE DISCRIMINANT. THIS TEST CONTAINS CHECKS FOR SUBTYPE 
-- INDICATIONS WHERE THE TYPE MARK DENOTES A PRIVATE OR LIMITED 
-- PRIVATE TYPE, THE DISCRIMINANT CONSTRAINT OCCURS BEFORE THE FULL 
-- DECLARATION OF THE TYPE, AND THERE ARE NO COMPONENTS OF THE TYPE 
-- DEPENDENT ON THE DISCRIMINANT.

-- R.WILLIAMS 8/28/86
-- EDS        7/14/98    AVOID OPTIMIZATION

WITH REPORT; USE REPORT;
PROCEDURE C37211C IS

     GLOBAL : BOOLEAN;

     SUBTYPE LIES IS BOOLEAN RANGE FALSE .. FALSE;

     FUNCTION SWITCH (B : BOOLEAN) RETURN BOOLEAN IS
     BEGIN
          GLOBAL := B;
          RETURN B;
     END SWITCH;
     
BEGIN
     TEST ( "C37211C", "CHECK THAT CONSTRAINT_ERROR IS RAISED BY " &
                       "A DISCRIMINANT CONSTRAINT IF A VALUE " &
                       "SPECIFIED FOR A DISCRIMINANT DOES NOT LIE " &
                       "IN THE RANGE OF THE DISCRIMINANT WHERE THE " &
                       "TYPE MARK DENOTES A PRIVATE OR LIMITED " &
                       "PRIVATE TYPE, AND THE DISCRIMINANT " &
                       "CONSTRAINT OCCURS BEFORE THE FULL " &
                       "DECLARATION OF THE TYPE" );
                         
     BEGIN
          DECLARE
                                   
               B1 : BOOLEAN := SWITCH (TRUE);

               PACKAGE PP IS
                    TYPE PRIV1 (D : LIES) IS PRIVATE;
                    SUBTYPE SUBPRIV IS PRIV1 (IDENT_BOOL (TRUE));

                    B2 : BOOLEAN := SWITCH (FALSE);

               PRIVATE
                    TYPE PRIV1 (D : LIES) IS
                         RECORD
                              NULL;
                         END RECORD;
               END PP;
                    
               USE PP;
          BEGIN
               DECLARE
                    SP : SUBPRIV;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF SUBTYPE SUBPRIV " & BOOLEAN'IMAGE(SP.D));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT SP" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF GLOBAL THEN 
                    NULL;
               ELSE
                    FAILED ( "EXCEPTION RAISED AT ELABORATION OF " &
                             "FULL TYPE PRIV1 NOT SUBTYPE SUBPRIV" );
               END IF;                    
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "SUBTYPE SUBPRIV" );
     END;

     BEGIN
          DECLARE
                                   
               B1 : BOOLEAN := SWITCH (TRUE);

               PACKAGE PL IS
                    TYPE LIM1 (D : LIES) IS LIMITED PRIVATE;
                    SUBTYPE SUBLIM IS LIM1 (IDENT_BOOL (TRUE));

                    B2 : BOOLEAN := SWITCH (FALSE);

               PRIVATE
                    TYPE LIM1 (D : LIES) IS
                         RECORD
                              NULL;
                         END RECORD;
               END PL;
                    
               USE PL;
          BEGIN
               DECLARE
                    SL : SUBLIM;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF SUBTYPE SUBLIM " & BOOLEAN'IMAGE(SL.D));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT SL" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF GLOBAL THEN 
                    NULL;
               ELSE
                    FAILED ( "EXCEPTION RAISED AT ELABORATION OF " &
                             "FULL TYPE LIM1 NOT SUBTYPE SUBLIM" );
               END IF;                    
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "SUBTYPE SUBLIM" );
     END;

     BEGIN
          DECLARE
               B1 : BOOLEAN := SWITCH (TRUE);

               PACKAGE PP IS
                    TYPE PRIV2 (D : LIES) IS PRIVATE;
                    TYPE PARR IS ARRAY (1 .. 5) OF 
                         PRIV2 (IDENT_BOOL (TRUE));
     
                    B2 : BOOLEAN := SWITCH (FALSE);

               PRIVATE
                    TYPE PRIV2 (D : LIES) IS
                         RECORD
                              NULL;
                         END RECORD;
               END PP;
                    
               USE PP;
          BEGIN
               DECLARE
                    PAR : PARR;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF TYPE PARR " & BOOLEAN'IMAGE(PAR(1).D));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT PAR" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF GLOBAL THEN 
                    NULL;
               ELSE
                    FAILED ( "EXCEPTION RAISED AT ELABORATION OF " &
                             "FULL TYPE PRIV2 NOT TYPE PARR" );
               END IF;                    
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "TYPE PARR" );
     END;
          
     BEGIN
          DECLARE
               B1 : BOOLEAN := SWITCH (TRUE);

               PACKAGE PL IS
                    TYPE LIM2 (D : LIES) IS LIMITED PRIVATE;
                    TYPE LARR IS ARRAY (1 .. 5) OF 
                         LIM2 (IDENT_BOOL (TRUE));
     
                    B2 : BOOLEAN := SWITCH (FALSE);

               PRIVATE
                    TYPE LIM2 (D : LIES) IS
                         RECORD
                              NULL;
                         END RECORD;
               END PL;
                    
               USE PL;
          BEGIN
               DECLARE
                    LAR : LARR;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF TYPE LARR " & BOOLEAN'IMAGE(LAR(1).D));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT LAR" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF GLOBAL THEN 
                    NULL;
               ELSE
                    FAILED ( "EXCEPTION RAISED AT ELABORATION OF " &
                             "FULL TYPE LIM2 NOT TYPE LARR" );
               END IF;                    
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "TYPE LARR" );
     END;
          
     BEGIN
          DECLARE
               B1 : BOOLEAN := SWITCH (TRUE);

               PACKAGE PP IS
                    TYPE PRIV3 (D : LIES) IS PRIVATE;

                    TYPE PRIV4 IS 
                         RECORD
                              X : PRIV3 (IDENT_BOOL (TRUE));
                         END RECORD;

                    B2 : BOOLEAN := SWITCH (FALSE);

               PRIVATE
                    TYPE PRIV3 (D : LIES) IS
                         RECORD
                              NULL;
                         END RECORD;
               END PP;
                    
               USE PP;
          BEGIN
               DECLARE
                    P4 : PRIV4;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF TYPE PRIV4 " & BOOLEAN'IMAGE(P4.X.D));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT P4" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF GLOBAL THEN 
                    NULL;
               ELSE
                    FAILED ( "EXCEPTION RAISED AT ELABORATION OF " &
                             "FULL TYPE PRIV3 NOT TYPE PRIV4" );
               END IF;                    
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "TYPE PRIV4" );
     END;
          
     BEGIN
          DECLARE
               B1 : BOOLEAN := SWITCH (TRUE);

               PACKAGE PL IS
                    TYPE LIM3 (D : LIES) IS LIMITED PRIVATE;

                    TYPE LIM4 IS 
                         RECORD
                              X : LIM3 (IDENT_BOOL (TRUE));
                         END RECORD;

                    B2 : BOOLEAN := SWITCH (FALSE);

               PRIVATE
                    TYPE LIM3 (D : LIES) IS
                         RECORD
                              NULL;
                         END RECORD;
               END PL;
                    
               USE PL;
          BEGIN
               DECLARE
                    L4 : LIM4;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF TYPE LIM4 " & BOOLEAN'IMAGE(L4.X.D));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT L4" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF GLOBAL THEN 
                    NULL;
               ELSE
                    FAILED ( "EXCEPTION RAISED AT ELABORATION OF " &
                             "FULL TYPE LIM3 NOT TYPE LIM4" );
               END IF;                    
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "TYPE LIM4" );
     END;
          
     BEGIN
          DECLARE
               B1 : BOOLEAN := SWITCH (TRUE);

               PACKAGE PP IS
                    TYPE PRIV5 (D : LIES) IS PRIVATE;
                    TYPE ACCPRIV IS ACCESS PRIV5 (IDENT_BOOL (TRUE));

                    B2 : BOOLEAN := SWITCH (FALSE);

               PRIVATE
                    TYPE PRIV5 (D : LIES) IS
                         RECORD
                              NULL;
                         END RECORD;
               END PP;
                    
               USE PP;

          BEGIN
               DECLARE
                    ACP : ACCPRIV;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF TYPE ACCPRIV " & BOOLEAN'IMAGE(ACP.D));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT ACP" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF GLOBAL THEN 
                    NULL;
               ELSE
                    FAILED ( "EXCEPTION RAISED AT ELABORATION OF " &
                             "FULL TYPE PRIV5 NOT TYPE ACCPRIV" );
               END IF;                    
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "TYPE ACCPRIV" );
     END;
          
     BEGIN
          DECLARE
               B1 : BOOLEAN := SWITCH (TRUE);

               PACKAGE PL IS
                    TYPE LIM5 (D : LIES) IS LIMITED PRIVATE;
                    TYPE ACCLIM IS ACCESS LIM5 (IDENT_BOOL (TRUE));

                    B2 : BOOLEAN := SWITCH (FALSE);

               PRIVATE
                    TYPE LIM5 (D : LIES) IS
                         RECORD
                              NULL;
                         END RECORD;
               END PL;
                    
               USE PL;

          BEGIN
               DECLARE
                    ACL : ACCLIM;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF TYPE ACCLIM " & BOOLEAN'IMAGE(ACL.D));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT ACL" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF GLOBAL THEN 
                    NULL;
               ELSE
                    FAILED ( "EXCEPTION RAISED AT ELABORATION OF " &
                             "FULL TYPE LIM5 NOT TYPE ACCLIM" );
               END IF;                    
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "TYPE ACCLIM" );
     END;
          
     RESULT;
END C37211C;
