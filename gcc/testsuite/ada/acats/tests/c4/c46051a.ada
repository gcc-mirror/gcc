-- C46051A.ADA

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
-- CHECK THAT ENUMERATION, RECORD, ACCESS, PRIVATE, AND TASK VALUES CAN
-- BE CONVERTED IF THE OPERAND AND TARGET TYPES ARE RELATED BY 
-- DERIVATION.

-- R.WILLIAMS 9/8/86

WITH REPORT; USE REPORT;
PROCEDURE C46051A IS
     
BEGIN
     TEST ( "C46051A", "CHECK THAT ENUMERATION, RECORD, ACCESS, " &
                       "PRIVATE, AND TASK VALUES CAN BE CONVERTED " &
                       "IF THE OPERAND AND TARGET TYPES ARE " &
                       "RELATED BY DERIVATION" );

     DECLARE
          TYPE ENUM IS (A, AB, ABC, ABCD);
          E : ENUM := ABC;

          TYPE ENUM1 IS NEW ENUM;
          E1 : ENUM1 := ENUM1'VAL (IDENT_INT (2));

          TYPE ENUM2 IS NEW ENUM;
          E2 : ENUM2 := ABC;

          TYPE NENUM1 IS NEW ENUM1;
          NE : NENUM1 := NENUM1'VAL (IDENT_INT (2));
     BEGIN
          IF ENUM (E) /= E THEN
               FAILED ( "INCORRECT CONVERSION OF 'ENUM (E)'" );
          END IF;

          IF ENUM (E1) /= E THEN
               FAILED ( "INCORRECT CONVERSION OF 'ENUM (E1)'" );
          END IF;

          IF ENUM1 (E2) /= E1 THEN
               FAILED ( "INCORRECT CONVERSION OF 'ENUM1 (E2)'" );
          END IF;
                              
          IF ENUM2 (NE) /= E2 THEN
               FAILED ( "INCORRECT CONVERSION OF 'ENUM2 (NE)'" );
          END IF;

          IF NENUM1 (E) /= NE THEN
               FAILED ( "INCORRECT CONVERSION OF 'NENUM (E)'" );
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED DURING CONVERSION OF " &
                        "ENUMERATION TYPES" );
     END;

     DECLARE
          TYPE REC IS 
               RECORD
                    NULL;
               END RECORD;

          R : REC;

          TYPE REC1 IS NEW REC;
          R1 : REC1;

          TYPE REC2 IS NEW REC;
          R2 : REC2;

          TYPE NREC1 IS NEW REC1;
          NR : NREC1;
     BEGIN
          IF REC (R) /= R THEN
               FAILED ( "INCORRECT CONVERSION OF 'REC (R)'" );
          END IF;

          IF REC (R1) /= R THEN
               FAILED ( "INCORRECT CONVERSION OF 'REC (R1)'" );
          END IF;

          IF REC1 (R2) /= R1 THEN
               FAILED ( "INCORRECT CONVERSION OF 'REC1 (R2)'" );
          END IF;
                              
          IF REC2 (NR) /= R2 THEN
               FAILED ( "INCORRECT CONVERSION OF 'REC2 (NR)'" );
          END IF;

          IF NREC1 (R) /= NR THEN
               FAILED ( "INCORRECT CONVERSION OF 'NREC (R)'" );
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED DURING CONVERSION OF " &
                        "RECORD TYPES" );
     END;

     DECLARE
          TYPE REC (D : INTEGER) IS 
               RECORD
                    NULL;
               END RECORD;

          SUBTYPE CREC IS REC (3);
          R : CREC;

          TYPE CREC1 IS NEW REC (3);
          R1 : CREC1;

          TYPE CREC2 IS NEW REC (3);
          R2 : CREC2;

          TYPE NCREC1 IS NEW CREC1;
          NR : NCREC1;
     BEGIN
          IF CREC (R) /= R THEN
               FAILED ( "INCORRECT CONVERSION OF 'CREC (R)'" );
          END IF;

          IF CREC (R1) /= R THEN
               FAILED ( "INCORRECT CONVERSION OF 'CREC (R1)'" );
          END IF;

          IF CREC1 (R2) /= R1 THEN
               FAILED ( "INCORRECT CONVERSION OF 'CREC1 (R2)'" );
          END IF;
                              
          IF CREC2 (NR) /= R2 THEN
               FAILED ( "INCORRECT CONVERSION OF 'CREC2 (NR)'" );
          END IF;

          IF NCREC1 (R) /= NR THEN
               FAILED ( "INCORRECT CONVERSION OF 'NCREC (R)'" );
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED DURING CONVERSION OF " &
                        "RECORD TYPES WITH DISCRIMINANTS" );
     END;

     DECLARE
          TYPE REC IS 
               RECORD
                    NULL;
               END RECORD;

          TYPE ACCREC IS ACCESS REC;
          AR : ACCREC;

          TYPE ACCREC1 IS NEW ACCREC;
          AR1 : ACCREC1;

          TYPE ACCREC2 IS NEW ACCREC;
          AR2 : ACCREC2;

          TYPE NACCREC1 IS NEW ACCREC1;
          NAR : NACCREC1;

          FUNCTION F (A : ACCREC) RETURN INTEGER IS
          BEGIN
               RETURN IDENT_INT (0);
          END F;

          FUNCTION F (A : ACCREC1) RETURN INTEGER IS
          BEGIN
               RETURN IDENT_INT (1);
          END F;

          FUNCTION F (A : ACCREC2) RETURN INTEGER IS
          BEGIN
               RETURN IDENT_INT (2);
          END F;

          FUNCTION F (A : NACCREC1) RETURN INTEGER IS
          BEGIN
               RETURN IDENT_INT (3);
          END F;

     BEGIN
          IF F (ACCREC (AR)) /= 0 THEN
               FAILED ( "INCORRECT CONVERSION OF 'ACCREC (AR)'" );
          END IF;

          IF F (ACCREC (AR1)) /= 0 THEN
               FAILED ( "INCORRECT CONVERSION OF 'ACCREC (AR1)'" );
          END IF;

          IF F (ACCREC1 (AR2)) /= 1 THEN
               FAILED ( "INCORRECT CONVERSION OF 'ACCREC1 (AR2)'" );
          END IF;
                              
          IF F (ACCREC2 (NAR)) /= 2 THEN
               FAILED ( "INCORRECT CONVERSION OF 'ACCREC2 (NAR)'" );
          END IF;

          IF F (NACCREC1 (AR)) /= 3 THEN
               FAILED ( "INCORRECT CONVERSION OF 'NACCREC (AR)'" );
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED DURING CONVERSION OF " &
                        "ACCESS TYPES" );
     END;

     DECLARE
          TYPE REC (D : INTEGER) IS 
               RECORD
                    NULL;
               END RECORD;

          TYPE ACCR IS ACCESS REC;

          SUBTYPE CACCR IS ACCR (3);
          AR : CACCR;

          TYPE CACCR1 IS NEW ACCR (3);
          AR1 : CACCR1;

          TYPE CACCR2 IS NEW ACCR (3);
          AR2 : CACCR2;

          TYPE NCACCR1 IS NEW CACCR1;
          NAR : NCACCR1;

          FUNCTION F (A : CACCR) RETURN INTEGER IS
          BEGIN
               RETURN IDENT_INT (0);
          END F;

          FUNCTION F (A : CACCR1) RETURN INTEGER IS
          BEGIN
               RETURN IDENT_INT (1);
          END F;

          FUNCTION F (A : CACCR2) RETURN INTEGER IS
          BEGIN
               RETURN IDENT_INT (2);
          END F;

          FUNCTION F (A : NCACCR1) RETURN INTEGER IS
          BEGIN
               RETURN IDENT_INT (3);
          END F;

     BEGIN
          IF F (CACCR (AR)) /= 0 THEN
               FAILED ( "INCORRECT CONVERSION OF 'CACCR (AR)'" );
          END IF;

          IF F (CACCR (AR1)) /= 0 THEN
               FAILED ( "INCORRECT CONVERSION OF 'CACCR (AR1)'" );
          END IF;

          IF F (CACCR1 (AR2)) /= 1 THEN
               FAILED ( "INCORRECT CONVERSION OF 'CACCR1 (AR2)'" );
          END IF;
                              
          IF F (CACCR2 (NAR)) /= 2 THEN
               FAILED ( "INCORRECT CONVERSION OF 'CACCR2 (NAR)'" );
          END IF;

          IF F (NCACCR1 (AR)) /= 3 THEN
               FAILED ( "INCORRECT CONVERSION OF 'NCACCR (AR)'" );
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED DURING CONVERSION OF " &
                        "CONSTRAINED ACCESS TYPES" );
     END;

     DECLARE
          PACKAGE PKG1 IS
               TYPE PRIV IS PRIVATE;
          PRIVATE
               TYPE PRIV IS 
                    RECORD
                         NULL;
                    END RECORD;
          END PKG1;

          USE PKG1;

          PACKAGE PKG2 IS
               R : PRIV;

               TYPE PRIV1 IS NEW PRIV;
               R1 : PRIV1;

               TYPE PRIV2 IS NEW PRIV;
               R2 : PRIV2;
          END PKG2;
     
          USE PKG2;

          PACKAGE PKG3 IS
               TYPE NPRIV1 IS NEW PRIV1;
               NR : NPRIV1;
          END PKG3;

          USE PKG3;
     BEGIN
          IF PRIV (R) /= R THEN
               FAILED ( "INCORRECT CONVERSION OF 'PRIV (R)'" );
          END IF;

          IF PRIV (R1) /= R THEN
               FAILED ( "INCORRECT CONVERSION OF 'PRIV (R1)'" );
          END IF;

          IF PRIV1 (R2) /= R1 THEN
               FAILED ( "INCORRECT CONVERSION OF 'PRIV1 (R2)'" );
          END IF;
                              
          IF PRIV2 (NR) /= R2 THEN
               FAILED ( "INCORRECT CONVERSION OF 'PRIV2 (NR)'" );
          END IF;

          IF NPRIV1 (R) /= NR THEN
               FAILED ( "INCORRECT CONVERSION OF 'NPRIV (R)'" );
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED DURING CONVERSION OF " &
                        "PRIVATE TYPES" );
     END;

     DECLARE
          TASK TYPE TK;
          T : TK;

          TYPE TK1 IS NEW TK;
          T1 : TK1; 

          TYPE TK2 IS NEW TK;
          T2 : TK2;

          TYPE NTK1 IS NEW TK1;
          NT : NTK1;
          
          TASK BODY TK IS
          BEGIN
               NULL;
          END;

          FUNCTION F (T : TK) RETURN INTEGER IS
          BEGIN
               RETURN IDENT_INT (0);
          END F;

          FUNCTION F (T : TK1) RETURN INTEGER IS
          BEGIN
               RETURN IDENT_INT (1);
          END F;

          FUNCTION F (T : TK2) RETURN INTEGER IS
          BEGIN
               RETURN IDENT_INT (2);
          END F;

          FUNCTION F (T : NTK1) RETURN INTEGER IS
          BEGIN
               RETURN IDENT_INT (3);
          END F;

     BEGIN
          IF F (TK (T)) /= 0 THEN
               FAILED ( "INCORRECT CONVERSION OF 'TK (T))'" );
          END IF;

          IF F (TK (T1)) /= 0 THEN
               FAILED ( "INCORRECT CONVERSION OF 'TK (T1))'" );
          END IF;

          IF F (TK1 (T2)) /= 1 THEN
               FAILED ( "INCORRECT CONVERSION OF 'TK1 (T2))'" );
          END IF;
                              
          IF F (TK2 (NT)) /= 2 THEN
               FAILED ( "INCORRECT CONVERSION OF 'TK2 (NT))'" );
          END IF;

          IF F (NTK1 (T)) /= 3 THEN
               FAILED ( "INCORRECT CONVERSION OF 'NTK (T))'" );
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED DURING CONVERSION OF " &
                        "TASK TYPES" );
     END;

     RESULT;
END C46051A;
