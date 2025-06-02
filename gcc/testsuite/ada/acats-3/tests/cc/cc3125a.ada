-- CC3125A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED IF THE INITIAL VALUE OF A 
-- GENERIC IN PARAMETER DOES NOT SATISFY ITS SUBTYPE CONSTRAINT.

-- THIS TEST CHECKS PARAMETERS OF A NON-GENERIC TYPE.

-- DAT 8/10/81
-- SPS 4/14/82

WITH REPORT; USE REPORT;

PROCEDURE CC3125A IS

BEGIN
     TEST ("CC3125A","GENERIC PARAMETER DEFAULTS OF " &
                     "NON-GENERIC TYPE EVALUATED AND CHECKED WHEN " &
                     "DECLARATION IS INSTANTIATED AND DEFAULT USED");

     FOR I IN 1 .. 3 LOOP
          COMMENT ("LOOP ITERATION");
          BEGIN

               DECLARE
                    SUBTYPE T IS INTEGER RANGE 1 .. IDENT_INT(1);
                    SUBTYPE I_1_2 IS INTEGER RANGE
                         IDENT_INT (1) .. IDENT_INT (2);

                    GENERIC
                         P,Q : T := I_1_2'(I);
                    PACKAGE PKG IS 
                         R: T := P;
                    END PKG;

               BEGIN

                    BEGIN
                         DECLARE
                              PACKAGE P1 IS NEW PKG;
                         BEGIN
                              IF I = IDENT_INT(1) THEN
                                   IF P1.R /= IDENT_INT(1) 
                                   THEN FAILED ("BAD INITIAL"&
                                        " VALUE");
                                   END IF;
                              ELSIF I = 2 THEN
                                   FAILED ("SUBTYPE NOT CHECKED AT " &
                                           "INSTANTIATION");
                              ELSE
                                   FAILED ("DEFAULT NOT EVALUATED AT " &
                                           "INSTANTIATION");
                              END IF;
                         EXCEPTION
                              WHEN OTHERS => FAILED ("WRONG HANDLER");
                         END;
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              CASE I IS
                                   WHEN 1 =>
                                        FAILED ("INCORRECT EXCEPTION");
                                   WHEN 2 =>
                                        COMMENT ("CONSTRAINT CHECKED" &
                                             " ON INSTANTIATION");
                                   WHEN 3 =>
                                        COMMENT ("DEFAULT EVALUATED " &
                                             "ON INSTANTIATION");
                              END CASE;
                    END;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION");
               END;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    CASE I IS
                         WHEN 1 =>
                              FAILED ("NO EXCEPTION SHOULD BE RAISED");
                         WHEN 2 =>
                              FAILED ("DEFAULT CHECKED AGAINST " &
                                      "SUBTYPE AT DECLARATION");
                         WHEN 3 =>
                              FAILED ("DEFAULT EVALUATED AT " &
                                      "DECLARATION");
                    END CASE;
          END;
     END LOOP;

     RESULT;
END CC3125A;
