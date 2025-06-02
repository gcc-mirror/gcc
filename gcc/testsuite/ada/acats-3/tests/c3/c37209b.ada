-- C37209B.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN THE SUBTYPE
--     INDICATION IN A CONSTANT OBJECT DECLARATION SPECIFIES A
--     CONSTRAINED SUBTYPE WITH DISCRIMINANTS AND THE INITIALIZATION
--     VALUE DOES NOT BELONG TO THE SUBTYPE (I. E., THE DISCRIMINANT
--     VALUE DOES NOT MATCH THOSE SPECIFIED BY THE CONSTRAINT).

-- HISTORY:
--     RJW 08/25/86  CREATED ORIGINAL TEST
--     VCL 08/19/87  CHANGED THE RETURN TYPE OF FUNTION 'INIT' IN
--                   PACKAGE 'PRIV2' SO THAT 'INIT' IS UNCONSTRAINED,
--                   THUS NOT RAISING A CONSTRAINT ERROR ON RETURN FROM
--                   'INIT'.

WITH REPORT; USE REPORT;
PROCEDURE C37209B IS

BEGIN
     TEST ( "C37209B", "CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN " &
                       "THE SUBTYPE INDICATION IN A CONSTANT " &
                       "OBJECT DECLARATION SPECIFIES A CONSTRAINED " &
                       "SUBTYPE WITH DISCRIMINANTS AND THE " &
                       "INITIALIZATION VALUE DOES NOT BELONG TO " &
                       "THE SUBTYPE (I. E., THE DISCRIMINANT VALUE " &
                       "DOES NOT MATCH THOSE SPECIFIED BY THE " &
                       "CONSTRAINT)" );
     DECLARE

          TYPE REC (D : INTEGER) IS
               RECORD
                    NULL;
               END RECORD;

          SUBTYPE REC1 IS REC (IDENT_INT (5));
     BEGIN
          DECLARE
               R1 : CONSTANT REC1 := (D => IDENT_INT (10));
               I  : INTEGER := IDENT_INT (R1.D);
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR DECLARATION OF " &
                        "R1" );
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ( "EXCEPTION FOR R1 RAISED INSIDE BLOCK" );
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED AT DECLARATION OF " &
                        "R1" );
     END;


     BEGIN
          DECLARE
               PACKAGE PRIV1 IS
                    TYPE REC (D : INTEGER) IS PRIVATE;
                    SUBTYPE REC2 IS REC (IDENT_INT (5));
                    R2 : CONSTANT REC2;

               PRIVATE
                    TYPE REC (D : INTEGER) IS
                         RECORD
                              NULL;
                         END RECORD;

                    R2 : CONSTANT REC2 := (D => IDENT_INT (10));
               END PRIV1;

               USE PRIV1;

          BEGIN
               DECLARE
                    I : INTEGER := IDENT_INT (R2.D);
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT DECLARATION " &
                             "OF R2" );
              END;
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED AT DECLARATION " &
                        "OF R2" );
     END;

     BEGIN
          DECLARE
               PACKAGE PRIV2 IS
                    TYPE REC (D : INTEGER) IS PRIVATE;
                    SUBTYPE REC3 IS REC (IDENT_INT (5));

                    FUNCTION INIT (D : INTEGER) RETURN REC;
               PRIVATE
                    TYPE REC (D : INTEGER) IS
                         RECORD
                              NULL;
                         END RECORD;

               END PRIV2;

               PACKAGE BODY PRIV2 IS
                    FUNCTION INIT (D : INTEGER) RETURN REC IS
                    BEGIN
                         RETURN (D => IDENT_INT (D));
                    END INIT;
               END PRIV2;

               USE PRIV2;

          BEGIN
               DECLARE
                    R3 : CONSTANT REC3 := INIT (10);
                    I  : INTEGER := IDENT_INT (R3.D);
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT DECLARATION " &
                             "OF R3" );
              END;
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED AT DECLARATION " &
                        "OF R3" );
     END;

     BEGIN
          DECLARE
               PACKAGE LPRIV IS
                    TYPE REC  (D : INTEGER) IS
                         LIMITED PRIVATE;
                    SUBTYPE REC4 IS REC (IDENT_INT (5));

                    R4 : CONSTANT REC4;

               PRIVATE
                    TYPE REC (D : INTEGER) IS
                         RECORD
                              NULL;
                         END RECORD;

                    R4 : CONSTANT REC4 := (D => IDENT_INT (10));
               END LPRIV;

               USE LPRIV;

          BEGIN
               DECLARE
                    I : INTEGER := IDENT_INT (R4.D);
               BEGIN
                    FAILED ( "NO EXCEPTION RAISED AT DECLARATION " &
                             "OF R4" );
              END;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED AT DECLARATION " &
                        "OF R4" );
     END;

     RESULT;
END C37209B;
