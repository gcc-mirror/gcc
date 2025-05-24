-- C64106B.ADA

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
-- CHECK THAT ASSIGNMENTS TO FORMAL PARAMETERS OF UNCONSTRAINED RECORD,
--   PRIVATE, AND LIMITED PRIVATE TYPES WITHOUT DEFAULT CONSTRAINTS
--   RAISE CONSTRAINT_ERROR IF AN ATTEMPT IS MADE TO CHANGE THE 
--   CONSTRAINT OF THE ACTUAL PARAMETER.
--   SUBTESTS ARE:
--        (A) RECORD TYPE.
--        (B) PRIVATE TYPE.
--        (C) LIMITED PRIVATE TYPE.

-- DAS  1/15/81
-- CPP  8/9/84

WITH REPORT;
PROCEDURE C64106B IS

     USE REPORT;

BEGIN

     TEST ("C64106B", "CHECK ASSIGNMENT TO FORMAL PARAMETERS OF " &
                      "UNCONSTRAINED TYPE (WITH NO DEFAULT)");

     --------------------------------------------------

     DECLARE  -- (A)

          PACKAGE PKG IS

               TYPE RECTYPE (CONSTRAINT : INTEGER) IS
                    RECORD
                         INTFIELD  : INTEGER;
                         STRFIELD  : STRING (1..CONSTRAINT);
                    END RECORD;

               PROCEDURE CHK_RECTYPE (REC9 : OUT RECTYPE;
                                      REC6 : IN OUT RECTYPE);
          END PKG;

          REC9 : PKG.RECTYPE(IDENT_INT(9))    :=
                 (IDENT_INT(9), 9, "123456789");
          REC6 : PKG.RECTYPE(IDENT_INT(6))    :=
                 (IDENT_INT(6), 5, "AEIOUY");

          PACKAGE BODY PKG IS

               PROCEDURE CHK_RECTYPE (REC9 : OUT RECTYPE;
                                      REC6 : IN OUT RECTYPE) IS

                    REC4 : CONSTANT RECTYPE(IDENT_INT(4)) :=
                           (IDENT_INT(4), 4, "OOPS"); 

               BEGIN
                    BEGIN  -- (A.1)
                         REC9 := REC6;
                         FAILED ("CONSTRAINT_ERROR NOT RAISED - A.1");
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED ("WRONG EXCEPTION RAISED - A.1");
                    END;   -- (A.1)

                    BEGIN  -- (A.2)
                         REC6 := REC4;
                         FAILED ("CONSTRAINT_ERROR NOT RAISED - A.2");
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED ("WRONG EXCEPTION RAISED - A.2");
                    END;   -- (A.2)

                    REC9 := (IDENT_INT(9), 9, "987654321");

               END CHK_RECTYPE;
          END PKG;

     BEGIN  -- (A)

          PKG.CHK_RECTYPE (REC9, REC6);
          IF REC9.STRFIELD /= IDENT_STR("987654321") THEN
               FAILED ("ASSIGNMENT TO REC9 FAILED - (A)");
          END IF;

     END;   -- (A)

     --------------------------------------------------

     DECLARE  -- (B)

          PACKAGE PKG IS

               TYPE RECTYPE (CONSTRAINT : INTEGER) IS PRIVATE;

               PROCEDURE CHK_RECTYPE (REC9 : OUT RECTYPE;
                                      REC6 : IN OUT RECTYPE);
          PRIVATE
               TYPE RECTYPE (CONSTRAINT : INTEGER) IS
                    RECORD
                         INTFIELD  : INTEGER;
                         STRFIELD  : STRING (1..CONSTRAINT);
                    END RECORD;
          END PKG;

          REC9 : PKG.RECTYPE(9);
          REC6 : PKG.RECTYPE(6);

          PACKAGE BODY PKG IS

               PROCEDURE CHK_RECTYPE (REC9 : OUT RECTYPE;
                                      REC6 : IN OUT RECTYPE) IS

                    REC4 : CONSTANT RECTYPE(4)  := (4, 4, "OOPS"); 

               BEGIN
                    BEGIN  -- (B.1)
                         REC9 := REC6;
                         FAILED ("CONSTRAINT_ERROR NOT RAISED - B.1");
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED ("WRONG EXCEPTION RAISED - B.1");
                    END;   -- (B.1)

                    BEGIN  -- (B.2)
                         REC6 := REC4;
                         FAILED ("CONSTRAINT_ERROR NOT RAISED - B.2");
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED ("WRONG EXCEPTION RAISED - B.2");
                    END;   -- (B.2)
               END CHK_RECTYPE;

          BEGIN
               REC9 := (9, 9, "123456789");
               REC6 := (6, 5, "AEIOUY");
          END PKG;

     BEGIN  -- (B)

          PKG.CHK_RECTYPE (REC9, REC6);

     END;   -- (B)

     --------------------------------------------------

     DECLARE  -- (C)

          PACKAGE PKG IS

               TYPE RECTYPE (CONSTRAINT : INTEGER) IS LIMITED PRIVATE;

               PROCEDURE CHK_RECTYPE (REC9 : OUT RECTYPE;
                                      REC6 : IN OUT RECTYPE);
          PRIVATE
               TYPE RECTYPE (CONSTRAINT : INTEGER) IS
                    RECORD
                         INTFIELD  : INTEGER;
                         STRFIELD  : STRING (1..CONSTRAINT);
                    END RECORD;
          END PKG;

          REC6 : PKG.RECTYPE(IDENT_INT(6));
          REC9 : PKG.RECTYPE(IDENT_INT(9));

          PACKAGE BODY PKG IS

               PROCEDURE CHK_RECTYPE (REC9 : OUT RECTYPE;
                                      REC6 : IN OUT RECTYPE) IS

                    REC4 : CONSTANT RECTYPE(4)  := (4, 4, "OOPS"); 

               BEGIN
                    BEGIN  -- (C.1)
                         REC9 := REC6;
                         FAILED ("CONSTRAINT_ERROR NOT RAISED - C.1");
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED ("WRONG EXCEPTION RAISED - C.1");
                    END;   -- (C.1)

                    BEGIN  -- (C.2)
                         REC6 := REC4;
                         FAILED ("CONSTRAINT_ERROR NOT RAISED - C.2");
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED ("WRONG EXCEPTION RAISED - C.2");
                    END;   -- (C.2)
               END CHK_RECTYPE;

          BEGIN
               REC6 := (6, 5, "AEIOUY");
               REC9 := (9, 9, "123456789");
          END PKG;

     BEGIN  -- (C)

          PKG.CHK_RECTYPE (REC9, REC6);

     END;   -- (C)

     --------------------------------------------------

     RESULT;

END C64106B;
