-- CC1226B.ADA

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
--     CHECK, FOR A FORMAL NONLIMITED PRIVATE TYPE, THAT ALL ALLOWABLE
--     OPERATIONS ARE IMPLICITLY DECLARED.

-- HISTORY:
--     BCB 04/04/88  CREATED ORIGINAL TEST.
--     RJW 03/28/90  INITIALIZED PREVIOUSLY UNINITIALIZED VARIABLES.
--     LDC 09/19/90  INITALIZED NLPVAR & NLPVAR2 TO DIFFERENT VALUES,
--                   REMOVED USE CLAUSE.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;

PROCEDURE CC1226B IS

     TYPE DISCREC(DISC1 : INTEGER := 1;
                  DISC2 : BOOLEAN := FALSE) IS RECORD
          NULL;
     END RECORD;

     GENERIC
          TYPE NLP IS PRIVATE;
          TYPE NLPDISC(DISC1 : INTEGER;
                       DISC2 : BOOLEAN) IS PRIVATE;
          WITH PROCEDURE INITIALIZE (N : OUT NLPDISC);
          WITH FUNCTION INITIALIZE RETURN NLP;
          WITH FUNCTION INITIALIZE_2 RETURN NLP;
     PACKAGE P IS
          FUNCTION IDENT(X : NLP) RETURN NLP;
          FUNCTION IDENT_ADR(Y : ADDRESS) RETURN ADDRESS;
     END P;

     PACKAGE BODY P IS
          TYPE DER_NLP IS NEW NLP;
          NLPVAR : NLP := INITIALIZE_2;
          NLPVAR2, NLPVAR3 : NLP := INITIALIZE;
          DERNLP : DER_NLP := DER_NLP (INITIALIZE);
          NDVAR : NLPDISC(DISC1 => 5, DISC2 => TRUE);
          NLPVARADDRESS : ADDRESS;
          NLPSIZE : INTEGER;
          NLPBASESIZE : INTEGER;

          FUNCTION IDENT(X : NLP) RETURN NLP IS
               Z : NLP := INITIALIZE;
          BEGIN
               IF EQUAL(3,3) THEN
                    RETURN X;
               END IF;
               RETURN Z;
          END IDENT;

          FUNCTION IDENT_ADR(Y : ADDRESS) RETURN ADDRESS IS
               I : INTEGER;
               Z : ADDRESS := I'ADDRESS;
          BEGIN
               IF EQUAL(3,3) THEN
                    RETURN Y;
               END IF;
               RETURN Z;
          END IDENT_ADR;

     BEGIN
          TEST ("CC1226B", "CHECK, FOR A FORMAL NONLIMITED PRIVATE " &
                           "TYPE THAT ALL ALLOWABLE OPERATIONS ARE " &
                           "IMPLICITLY DECLARED");

          INITIALIZE (NDVAR);

          NLPVAR := NLPVAR2;

          IF NLPVAR /= NLPVAR2 THEN
               FAILED ("IMPROPER VALUE FROM ASSIGNMENT");
          END IF;

          IF NLPVAR NOT IN NLP THEN
               FAILED ("IMPROPER RESULT FROM MEMBERSHIP TEST");
          END IF;

          NLPVAR := NLP'(NLPVAR2);

          IF NLPVAR /= NLPVAR2 THEN
               FAILED ("IMPROPER RESULT FROM QUALIFICATION");
          END IF;

          NLPVAR := NLP(DERNLP);

          IF NLPVAR /= IDENT(NLP(DERNLP)) THEN
               FAILED ("IMPROPER RESULT FROM EXPLICIT CONVERSION");
          END IF;

          NLPSIZE := IDENT_INT(NLP'SIZE);

          IF NLPSIZE /= INTEGER(NLP'SIZE) THEN
               FAILED ("IMPROPER VALUE FOR NLP'SIZE");
          END IF;

          NLPVARADDRESS := NLPVAR'ADDRESS;

          IF NLPVAR'ADDRESS /= IDENT_ADR(NLPVARADDRESS) THEN
               FAILED ("IMPROPER VALUE FOR NLPVAR'ADDRESS");
          END IF;

          IF NDVAR.DISC1 /= IDENT_INT(5) THEN
               FAILED ("IMPROPER DISCRIMINANT VALUE - 1");
          END IF;

          IF NOT NDVAR.DISC2 THEN
               FAILED ("IMPROPER DISCRIMINANT VALUE - 2");
          END IF;

          IF NOT NDVAR'CONSTRAINED THEN
               FAILED ("IMPROPER VALUE FOR NDVAR'CONSTRAINED");
          END IF;

          NLPVAR := NLPVAR3;

          IF NOT (NLPVAR = IDENT(NLPVAR3)) THEN
               FAILED ("IMPROPER VALUE FROM EQUALITY OPERATION");
          END IF;

          IF NLPVAR /= IDENT(NLPVAR3) THEN
               FAILED ("IMPROPER VALUE FROM INEQUALITY OPERATION");
          END IF;

          RESULT;
     END P;

     PROCEDURE INITIALIZE (I : OUT DISCREC) IS
     BEGIN
          I := (5, TRUE);
     END INITIALIZE;

     FUNCTION INITIALIZE RETURN INTEGER IS
     BEGIN
          RETURN 5;
     END INITIALIZE;

     FUNCTION INITIALIZE_OTHER RETURN INTEGER IS
     BEGIN
          RETURN 3;
     END INITIALIZE_OTHER;

     PACKAGE PACK IS NEW P(INTEGER, 
                           DISCREC, 
                           INITIALIZE, 
                           INITIALIZE, 
                           INITIALIZE_OTHER);

BEGIN
     NULL;
END CC1226B;
