-- CC1302A.ADA

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
-- CHECK THAT GENERIC DEFAULT SUBPROGRAM PARAMETERS MAY BE ATTRIBUTES
-- OF TYPES, INCLUDING GENERIC FORMAL TYPES IN SAME GENERIC PART,
-- OR IN GENERIC PART OF ENCLOSING UNIT.

-- DAT 8/27/81
-- SPS 2/9/83
-- JBG 2/15/83
-- JBG 4/29/83

WITH REPORT; USE REPORT;

PROCEDURE CC1302A IS
BEGIN
     TEST ("CC1302A", "GENERIC DEFAULT SUBPROGRAMS MAY BE"
          & " FUNCTION ATTRIBUTES OF TYPES");

     DECLARE
          GENERIC 
               TYPE T IS ( <> );
               T_LAST : T;
               WITH FUNCTION SUCC (X : T) RETURN T IS T'SUCC;
          PACKAGE PK1 IS
          END PK1;

          SUBTYPE CH IS CHARACTER RANGE CHARACTER'FIRST .. '~';
          SUBTYPE BL IS BOOLEAN RANGE FALSE .. FALSE;
          SUBTYPE INT IS INTEGER RANGE -10 .. 10;

          PACKAGE BODY PK1 IS
               GENERIC 
                    TYPE TT IS ( <> );
                    TT_LAST : TT;
                    WITH FUNCTION PRED (X : TT) RETURN TT IS TT'PRED;
                    WITH FUNCTION IM(X : T) RETURN STRING IS T'IMAGE;
                    WITH FUNCTION VAL(X : STRING) RETURN TT IS TT'VALUE;
               PACKAGE PK2 IS END PK2;

               PACKAGE BODY PK2 IS
               BEGIN

-- CHECK THAT 'LAST GIVES RIGHT ANSWER
                    IF T'LAST /= T_LAST THEN
                         FAILED ("T'LAST INCORRECT");
                    END IF;

                    IF TT'LAST /= TT_LAST THEN
                         FAILED ("TT'LAST INCORRECT");
                    END IF;

-- CHECK SUCC FUNCTION
                    BEGIN
                         IF T'PRED(SUCC(T'LAST)) /= T'LAST THEN
                              FAILED ("'PRED OR SUCC GIVES WRONG " &
                                      "RESULT");
                         END IF;
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              FAILED ("SUCC HAS CONSTRAINTS OF " &
                                      "SUBTYPE");
                         WHEN OTHERS =>
                              FAILED ("SOME EXCEPTION RAISED - 1");
                    END;

-- CHECK 'SUCC ATTRIBUTE
                    BEGIN
                         IF T'PRED(T'SUCC(T'LAST)) /= T'LAST THEN
                              FAILED ("'PRED OR 'SUCC GIVES WRONG " &
                                      "RESULT");
                         END IF;
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              FAILED ("'PRED OR 'SUCC HAS CONSTRAINTS "&
                                      "OF SUBTYPE");
                         WHEN OTHERS =>
                              FAILED ("SOME EXCEPTION RAISED - 2");
                    END;

-- CHECK VAL ATTRIBUTE
                    BEGIN
                         IF T'VAL(T'POS(T'SUCC(T'LAST))) /=
                            T'VAL(T'POS(T'LAST)+1) THEN
                              FAILED ("VAL OR POS ATTRIBUTE HAS " &
                                      "INCONSISTENT RESULTS");
                         END IF;
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              FAILED ("VAL OR POS ATTRIBUTE HAS " &
                                      "CONSTRAINTS OF SUBTYPE");
                         WHEN OTHERS =>
                              FAILED ("SOME EXCEPTION RAISED - 4");
                    END;

-- CHECK VAL FUNCTION
                    BEGIN
                         IF TT'VAL(TT'POS(TT'SUCC(TT'LAST))) /= 
                            TT'VAL(TT'POS(TT'LAST)+1) THEN
                              FAILED ("VAL FUNCTION GIVES INCORRECT " &
                                      "RESULTS");
                         END IF;
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              FAILED ("VAL FUNCTION HAS CONSTRAINTS " &
                                      "OF SUBTYPE");
                         WHEN OTHERS =>
                              FAILED ("SOME EXCEPTION RAISED - 6");
                    END;

-- CHECK IM FUNCTION
                    BEGIN
                         IF T'IMAGE(T'SUCC(T'LAST)) /= 
                              IM   (T'SUCC(T'LAST)) THEN
                              FAILED ("IM FUNCTION GIVES INCORRECT " &
                                      "RESULTS");
                         END IF;
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              FAILED ("IM FUNCTION HAS CONSTRAINTS " &
                                      "OF SUBTYPE");
                         WHEN OTHERS =>
                              FAILED ("SOME EXCEPTION RAISED - 7");
                    END;

-- CHECK PRED FUNCTION
                    BEGIN
                         IF PRED(TT'SUCC(TT'LAST)) /= TT'LAST THEN
                              FAILED ("PRED FUNCTION GIVES INCORRECT " &
                                      "RESULTS");
                         END IF;
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              FAILED ("PRED FUNCTION HAS CONSTRAINTS " &
                                      "OF SUBTYPE");
                         WHEN OTHERS =>
                              FAILED ("SOME EXCEPTION RAISED - 8");
                    END;

               END PK2;

               PACKAGE PK3 IS NEW PK2 (T, T'LAST);
          END PK1;

          PACKAGE PKG1 IS NEW PK1 (CH, CH'LAST);
          PACKAGE PKG2 IS NEW PK1 (BL, BL'LAST);
          PACKAGE PKG3 IS NEW PK1 (INT, INT'LAST);
     BEGIN
          NULL;
     END;

     RESULT;
END CC1302A;
