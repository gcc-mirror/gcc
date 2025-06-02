-- CC3127A.ADA

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
--     FOR A CONSTRAINED IN FORMAL PARAMETER HAVING A RECORD OR PRIVATE
--     TYPE WITH DISCRIMINANTS, CHECK THAT CONSTRAINT_ERROR IS RAISED
--     IF AND ONLY IF CORRESPONDING DISCRIMINANTS OF THE ACTUAL AND
--     FORMAL PARAMETER DO NOT HAVE THE SAME VALUES.

-- HISTORY:
--     LB   12/04/86  CREATED ORIGINAL TEST.
--     VCL  08/19/87  CORRECTED THE FORMAT OF THIS HEADER.

WITH REPORT; USE REPORT;

PROCEDURE  CC3127A  IS

     TYPE INT IS RANGE 1 .. 20;

BEGIN
     TEST ("CC3127A","CORRESPONDING DISCRIMINANTS OF THE GENERIC "&
                     "ACTUAL PARAMETER AND THE GENERIC FORMAL "&
                     "PARAMETER MUST HAVE THE SAME VALUES.");
     BEGIN
          DECLARE
               TYPE REC (A : INT) IS
                    RECORD
                         RINT : POSITIVE := 2;
                    END RECORD;
               SUBTYPE CON_REC IS REC(4);

               GENERIC
                    GREC : IN CON_REC;
               PACKAGE PA IS
                    NREC : CON_REC := GREC;
               END PA;
          BEGIN
               BEGIN
                    DECLARE
                         RVAR : REC(3);
                         PACKAGE AB IS NEW PA(RVAR);
                    BEGIN
                         FAILED ("EXCEPTION NOT RAISED 1");
                         AB.NREC.RINT := IDENT_INT(AB.NREC.RINT);
                    END;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED 1");
               END;

               BEGIN
                    DECLARE
                         SVAR : REC(4);
                         PACKAGE CD IS NEW PA(SVAR);
                    BEGIN
                         IF EQUAL(3,3) THEN
                              CD.NREC.RINT := IDENT_INT(CD.NREC.RINT);
                         END IF;
                    END;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED 2");
               END;
          END;

          DECLARE
               PACKAGE EF IS
                    TYPE PRI_REC (G : INT) IS PRIVATE;
               PRIVATE
                    TYPE PRI_REC (G : INT) IS
                         RECORD
                              PINT : POSITIVE := 2;
                         END RECORD;
               END EF;
               SUBTYPE CPRI_REC IS EF.PRI_REC(4);

               GENERIC
                    GEN_REC : IN CPRI_REC;
               PACKAGE GH IS
                    NGEN_REC : CPRI_REC := GEN_REC;
               END GH;

          BEGIN
               BEGIN
                    DECLARE
                         PVAR : EF.PRI_REC(4);
                         PACKAGE LM IS NEW GH(PVAR);
                    BEGIN
                         IF EQUAL(3,3) THEN
                              LM.NGEN_REC := LM.NGEN_REC;
                         END IF;
                    END;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED 3");
               END;

               BEGIN
                    DECLARE
                         PTVAR : EF.PRI_REC(5);
                         PACKAGE PAC IS NEW GH(PTVAR);
                    BEGIN
                         FAILED ("EXCEPTION NOT RAISED 4");
                         IF EQUAL(3,5) THEN
                            COMMENT ("DISCRIMINANT OF PAC.NGEN_REC IS "&
                                       INT'IMAGE(PAC.NGEN_REC.G));
                         END IF;
                    END;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED 4");
               END;
          END;
     END;

     RESULT;

END CC3127A;
