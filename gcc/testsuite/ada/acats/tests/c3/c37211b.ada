-- C37211B.ADA

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
-- PRIVATE TYPE, AND THE DISCRIMINANT CONSTRAINT OCCURS AFTER THE FULL 
-- DECLARATION OF THE TYPE.

-- R.WILLIAMS 8/28/86
-- EDS        7/14/98    AVOID OPTIMIZATION

WITH REPORT; USE REPORT;
PROCEDURE C37211B IS

     SUBTYPE LIES IS BOOLEAN RANGE FALSE .. FALSE;
     
     PACKAGE PKG IS
          TYPE PRIV (L : LIES) IS PRIVATE;
          TYPE LIM  (L : LIES) IS LIMITED PRIVATE;

     PRIVATE
          TYPE PRIV (L : LIES) IS 
               RECORD
                    NULL;
               END RECORD;

          TYPE LIM (L : LIES) IS
               RECORD
                    NULL;
               END RECORD;
     END PKG;     
     
     USE PKG;

BEGIN
     TEST ( "C37211B", "CHECK THAT CONSTRAINT_ERROR IS RAISED BY " &
                       "A DISCRIMINANT CONSTRAINT IF A VALUE " &
                       "SPECIFIED FOR A DISCRIMINANT DOES NOT LIE " &
                       "IN THE RANGE OF THE DISCRIMINANT WHERE THE " &
                       "TYPE MARK DENOTES A PRIVATE OR LIMITED " &
                       "PRIVATE TYPE, AND THE DISCRIMINANT " &
                       "CONSTRAINT OCCURS AFTER THE FULL " & 
                       "DECLARATION OF THE TYPE" );
                         
     BEGIN
          DECLARE
               SUBTYPE SUBPRIV IS PRIV (IDENT_BOOL (TRUE));
          BEGIN
               DECLARE
                    SP : SUBPRIV;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF SUBTYPE SUBPRIV " & 
                             BOOLEAN'IMAGE(SP.L));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT SP" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "SUBTYPE SUBPRIV" );
     END;

     BEGIN
          DECLARE
               SUBTYPE SUBLIM IS LIM (IDENT_BOOL (TRUE));
          BEGIN
               DECLARE
                    SL : SUBLIM;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF SUBTYPE SUBLIM" & 
                             BOOLEAN'IMAGE(SL.L));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT SL " );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "SUBTYPE SUBLIM" );
     END;

     BEGIN
          DECLARE
               TYPE PARR IS ARRAY (1 .. 5) OF PRIV (IDENT_BOOL (TRUE));
          BEGIN
               DECLARE
                    PAR : PARR;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF TYPE PARR " & 
                             BOOLEAN'IMAGE(PAR(1).L));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT PAR" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "TYPE PARR" );
     END;
          
     BEGIN
          DECLARE
               TYPE LARR IS ARRAY (1 .. 10) OF LIM (IDENT_BOOL (TRUE));
          BEGIN
               DECLARE
                    LAR : LARR;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF TYPE LARR " & 
                             BOOLEAN'IMAGE(LAR(1).L));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT LAR" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "TYPE LARR" );
     END;
          
     BEGIN
          DECLARE
               TYPE PRIV1 IS 
                    RECORD
                         X : PRIV (IDENT_BOOL (TRUE));
                    END RECORD;

          BEGIN
               DECLARE
                    P1 : PRIV1;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF TYPE PRIV1 " & 
                             BOOLEAN'IMAGE(P1.X.L));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT P1" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "TYPE PRIV1" );
     END;
          
     BEGIN
          DECLARE
               TYPE LIM1 IS 
                    RECORD
                         X : LIM (IDENT_BOOL (TRUE));
                    END RECORD;

          BEGIN
               DECLARE
                    L1 : LIM1;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF TYPE LIM1 " & 
                             BOOLEAN'IMAGE(L1.X.L));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT L1" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "TYPE LIM1" );
     END;
          
     BEGIN
          DECLARE
               TYPE ACCPRIV IS ACCESS PRIV (IDENT_BOOL (TRUE));
          BEGIN
               DECLARE
                    ACP : ACCPRIV;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF TYPE ACCPRIV " & 
                             BOOLEAN'IMAGE(ACP.L));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT ACP" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "TYPE ACCPRIV" );
     END;
          
     BEGIN
          DECLARE
               TYPE ACCLIM IS ACCESS LIM (IDENT_BOOL (TRUE));
          BEGIN
               DECLARE
                    ACL : ACCLIM;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF TYPE ACCLIM " & 
                             BOOLEAN'IMAGE(ACL.L));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT ACL" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "TYPE ACCLIM" );
     END;
          
     BEGIN
          DECLARE
               TYPE NEWPRIV IS NEW PRIV (IDENT_BOOL (TRUE));
          BEGIN
               DECLARE
                    NP : NEWPRIV;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF TYPE NEWPRIV " & 
                             BOOLEAN'IMAGE(NP.L));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT NP" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "TYPE NEWPRIV" );
     END;

     BEGIN
          DECLARE
               TYPE NEWLIM IS NEW LIM (IDENT_BOOL (TRUE));
          BEGIN
               DECLARE
                    NL : NEWLIM;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "ELABORATION OF TYPE NEWLIM " & 
                             BOOLEAN'IMAGE(NL.L));
               END;
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED AT DECLARATION OF " &
                             "OBJECT NL" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT ELABORATION OF " &
                        "TYPE NEWLIM" );
     END;

     BEGIN
          DECLARE
               P : PRIV (IDENT_BOOL (TRUE));
          BEGIN
               FAILED ( "NO EXCEPTION RAISED AT THE DECLARATION OF " &
                        "P " & BOOLEAN'IMAGE(P.L));
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED INSIDE BLOCK " &
                             "CONTAINING P" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT DECLARATION OF " &
                        "P" );
     END;

     BEGIN
          DECLARE
               L : LIM (IDENT_BOOL (TRUE));
          BEGIN
               FAILED ( "NO EXCEPTION RAISED AT THE DECLARATION OF " &
                        "L " & BOOLEAN'IMAGE(L.L));
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED INSIDE BLOCK " &
                             "CONTAINING L" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED AT DECLARATION OF " &
                        "L" );
     END;

     BEGIN
          DECLARE
               TYPE PRIV_NAME IS ACCESS PRIV;
          BEGIN
               DECLARE
                    PN : PRIV_NAME := NEW PRIV (IDENT_BOOL (TRUE));
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "DECLARATION OF OBJECT PN " & 
                             BOOLEAN'IMAGE(PN.L));
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ( "EXCEPTION ATTEMPTING TO USE OBJECT" );
               END;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;               
               WHEN OTHERS => 
                    FAILED ( "WRONG EXCEPTION RAISED AT DECLARATION " &
                             "OF OBJECT PN" );
          END;
     EXCEPTION
          WHEN OTHERS => 
               FAILED ( "EXCEPTION RAISED AT ELABORATION OF TYPE " &
                        "PRIV_NAME" );
     END;

     BEGIN
          DECLARE
               TYPE LIM_NAME IS ACCESS LIM;
          BEGIN
               DECLARE
                    LN : LIM_NAME := NEW LIM (IDENT_BOOL (TRUE));
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "DECLARATION OF OBJECT LN " & 
                             BOOLEAN'IMAGE(LN.L));
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ( "EXCEPTION ATTEMPTING TO USE OBJECT" );
               END;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;               
               WHEN OTHERS => 
                    FAILED ( "WRONG EXCEPTION RAISED AT DECLARATION " &
                             "OF OBJECT LN" );
          END;
     EXCEPTION
          WHEN OTHERS => 
               FAILED ( "EXCEPTION RAISED AT ELABORATION OF TYPE " &
                        "LIM_NAME" );
     END;

     BEGIN
          DECLARE
               PACKAGE PP IS
                    TYPE BAD_PRIV (D : LIES := IDENT_BOOL (TRUE)) IS
                         PRIVATE;
               PRIVATE
                    TYPE BAD_PRIV (D : LIES := IDENT_BOOL (TRUE)) IS
                         RECORD
                              NULL;
                         END RECORD;
               END PP;

               USE PP;
          BEGIN
               DECLARE
                    BP : BAD_PRIV;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "DECLARATION OF OBJECT BP " & 
                             BOOLEAN'IMAGE(BP.D));
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ( "EXCEPTION ATTEMPTING TO USE OBJECT" );
               END;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;               
               WHEN OTHERS => 
                    FAILED ( "WRONG EXCEPTION RAISED AT DECLARATION " &
                             "OF OBJECT BP" );
          END;
     EXCEPTION
          WHEN OTHERS => 
               FAILED ( "EXCEPTION RAISED AT ELABORATION OF TYPE " &
                        "BAD_PRIV" );
     END;

     BEGIN
          DECLARE
               PACKAGE PL IS
                    TYPE BAD_LIM (D : LIES := IDENT_BOOL (TRUE)) IS
                         LIMITED PRIVATE;
               PRIVATE
                    TYPE BAD_LIM (D : LIES := IDENT_BOOL (TRUE)) IS
                         RECORD
                              NULL;
                         END RECORD;
               END PL;

               USE PL;
          BEGIN
               DECLARE
                    BL : BAD_LIM;
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT THE " &
                             "DECLARATION OF OBJECT BL " & 
                             BOOLEAN'IMAGE(BL.D));
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ( "EXCEPTION ATTEMPTING TO USE OBJECT" );
               END;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;               
               WHEN OTHERS => 
                    FAILED ( "WRONG EXCEPTION RAISED AT DECLARATION " &
                             "OF OBJECT BL" );
          END;
     EXCEPTION
          WHEN OTHERS => 
               FAILED ( "EXCEPTION RAISED AT ELABORATION OF TYPE " &
                        "BAD_LIM" );
     END;

     RESULT;
END C37211B;
