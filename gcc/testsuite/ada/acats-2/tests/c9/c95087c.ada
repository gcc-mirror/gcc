-- C95087C.ADA

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
-- CHECK THAT ASSIGNMENTS TO ENTRY FORMAL PARAMETERS OF UNCONSTRAINED
--    RECORD, PRIVATE, AND LIMITED PRIVATE TYPES WITH DEFAULT
--    CONSTRAINTS RAISE CONSTRAINT_ERROR IF THE ACTUAL PARAMETER IS
--    CONSTRAINED AND THE CONSTRAINT VALUES OF THE OBJECT BEING
--    ASSIGNED TO DO NOT SATISFY THOSE OF THE ACTUAL PARAMETER.

--    SUBTESTS ARE:
--        (A) CONSTRAINED ACTUAL PARAMETERS OF RECORD TYPE.
--        (B) CONSTRAINED ACTUAL PARAMETERS OF PRIVATE TYPE.
--        (C) CONSTRAINED ACTUAL PARAMETERS OF LIMITED PRIVATE TYPE.

-- RJW  1/15/86

WITH REPORT; USE REPORT;
PROCEDURE C95087C IS

BEGIN

     TEST ( "C95087C", "CHECK ASSIGNMENTS TO ENTRY FORMAL " &
                       "PARAMETERS OF UNCONSTRAINED TYPES " &
                       "(WITH DEFAULTS)" );

     --------------------------------------------------

     DECLARE  -- (A)

          PACKAGE PKG IS

               SUBTYPE INTRANGE IS INTEGER RANGE 0..31;

               TYPE RECTYPE (CONSTRAINT : INTRANGE := 15) IS
                    RECORD
                         INTFLD   : INTRANGE;
                         STRFLD   : STRING(1..CONSTRAINT);
                    END RECORD;

               REC91,REC92,REC93  : RECTYPE(9);
               REC_OOPS           : RECTYPE(4);

               TASK T IS
                    ENTRY E (REC1 : IN RECTYPE; REC2 : IN OUT RECTYPE;
                             REC3 : OUT RECTYPE);
               END T;

          END PKG;

          PACKAGE BODY PKG IS

               TASK BODY T IS
               BEGIN
                    ACCEPT E (REC1 : IN RECTYPE; REC2 : IN OUT RECTYPE;
                              REC3 : OUT RECTYPE) DO

                         IF (NOT REC1'CONSTRAINED) OR
                            (REC1.CONSTRAINT /= IDENT_INT(9)) THEN
                              FAILED ( "CONSTRAINT ON RECORD TYPE " &
                                       "IN PARAMETER NOT RECOGNIZED" );
                         END IF;

                         BEGIN  -- ASSIGNMENT TO IN OUT PARAMETER.
                              REC2 := REC_OOPS;
                              FAILED ( "CONSTRAINT_ERROR NOT " &
                                       "RAISED - A.1" );
                         EXCEPTION
                              WHEN CONSTRAINT_ERROR =>
                                   NULL;
                              WHEN OTHERS =>
                                   FAILED ( "WRONG EXCEPTION " &
                                            "RAISED - A.1" );
                         END;

                         BEGIN  -- ASSIGNMENT TO OUT PARAMETER.
                              REC3 := REC_OOPS;
                              FAILED ( "CONSTRAINT_ERROR NOT " &
                                       "RAISED - A.2" );
                         EXCEPTION
                              WHEN CONSTRAINT_ERROR =>
                                   NULL;
                              WHEN OTHERS =>
                                   FAILED ( "WRONG EXCEPTION " &
                                            "RAISED - A.2" );
                         END;

                    END E;
               END T;

          BEGIN

               REC91 := (9, 9, "123456789");
               REC92 := REC91;
               REC93 := REC91;

               REC_OOPS := (4, 4, "OOPS");

          END PKG;

     BEGIN  -- (A)

          PKG.T.E (PKG.REC91, PKG.REC92, PKG.REC93);

     END;   -- (A)

     --------------------------------------------------

     DECLARE  -- (B)

          PACKAGE PKG IS

               SUBTYPE INTRANGE IS INTEGER RANGE 0..31;

               TYPE RECTYPE (CONSTRAINT : INTRANGE := 15) IS PRIVATE;

               TASK T IS
                    ENTRY E (REC1 : IN RECTYPE; REC2 : IN OUT RECTYPE;
                             REC3 : OUT RECTYPE);
               END T;

          PRIVATE

               TYPE RECTYPE (CONSTRAINT : INTRANGE := 15) IS
                    RECORD
                         INTFLD   : INTRANGE;
                         STRFLD   : STRING(1..CONSTRAINT);
                    END RECORD;
          END PKG;

          REC91, REC92, REC93  : PKG.RECTYPE(9);
          REC_OOPS             : PKG.RECTYPE(4);

          PACKAGE BODY PKG IS

               TASK BODY T IS
               BEGIN
                     ACCEPT E (REC1 : IN RECTYPE; REC2 : IN OUT RECTYPE;
                               REC3 : OUT RECTYPE) DO

                         IF (NOT REC1'CONSTRAINED) OR
                            (REC1.CONSTRAINT /= IDENT_INT(9)) THEN
                              FAILED ( "CONSTRAINT ON PRIVATE TYPE " &
                                       "IN PARAMETER NOT RECOGNIZED" );
                         END IF;

                         BEGIN  -- ASSIGNMENT TO IN OUT PARAMETER.
                              REC2 := REC_OOPS;
                              FAILED ( "CONSTRAINT_ERROR NOT " &
                                       "RAISED - B.1" );
                         EXCEPTION
                              WHEN CONSTRAINT_ERROR =>
                                   NULL;
                              WHEN OTHERS =>
                                   FAILED ( "WRONG EXCEPTION " &
                                            "RAISED - B.1" );
                         END;

                         BEGIN  -- ASSIGNMENT TO OUT PARAMETER.
                              REC3 := REC_OOPS;
                              FAILED ( "CONSTRAINT_ERROR NOT " &
                                       "RAISED - B.2" );
                         EXCEPTION
                              WHEN CONSTRAINT_ERROR =>
                                   NULL;
                              WHEN OTHERS =>
                                   FAILED ( "WRONG EXCEPTION " &
                                            "RAISED - B.2" );
                         END;

                    END E;
              END T;

          BEGIN

               REC91 := (9, 9, "123456789");
               REC92 := REC91;
               REC93 := REC91;

               REC_OOPS := (4, 4, "OOPS");

          END PKG;

     BEGIN  -- (B)

          PKG.T.E (REC91, REC92, REC93);

     END;   -- (B)

     --------------------------------------------------

     DECLARE  -- (C)

          PACKAGE PKG IS

               SUBTYPE INTRANGE IS INTEGER RANGE 0..31;

               TYPE RECTYPE (CONSTRAINT : INTRANGE := 15) IS
                    LIMITED PRIVATE;

               TASK T IS
                    ENTRY E (REC1 : IN RECTYPE; REC2 : IN OUT RECTYPE;
                             REC3 : OUT RECTYPE);
               END T;

          PRIVATE

               TYPE RECTYPE (CONSTRAINT : INTRANGE := 15) IS
                    RECORD
                         INTFLD   : INTRANGE;
                         STRFLD   : STRING(1..CONSTRAINT);
                    END RECORD;
          END PKG;

          REC91,REC92,REC93  : PKG.RECTYPE(9);
          REC_OOPS           : PKG.RECTYPE(4);

          PACKAGE BODY PKG IS

               TASK BODY T IS
               BEGIN
                    ACCEPT E (REC1 : IN RECTYPE; REC2 : IN OUT RECTYPE;
                              REC3 : OUT RECTYPE) DO

                         IF (NOT REC1'CONSTRAINED) OR
                            (REC1.CONSTRAINT /= 9) THEN
                              FAILED ( "CONSTRAINT ON LIMITED " &
                                       "PRIVATE TYPE IN PARAMETER " &
                                       "NOT RECOGNIZED" );
                         END IF;

                         BEGIN  -- ASSIGNMENT TO IN OUT PARAMETER.
                              REC2 := REC_OOPS;
                              FAILED ( "CONSTRAINT_ERROR NOT " &
                                       "RAISED - C.1" );
                         EXCEPTION
                              WHEN CONSTRAINT_ERROR =>
                                   NULL;
                              WHEN OTHERS =>
                                   FAILED ( "WRONG EXCEPTION " &
                                            "RAISED - C.1" );
                         END;

                         BEGIN  -- ASSIGNMENT TO OUT PARAMETER.
                              REC3 := REC_OOPS;
                              FAILED ( "CONSTRAINT_ERROR NOT RAISED " &
                                       "- C.2" );
                         EXCEPTION
                              WHEN CONSTRAINT_ERROR =>
                                   NULL;
                              WHEN OTHERS =>
                                   FAILED ( "WRONG EXCEPTION " &
                                            "RAISED - C.2" );
                         END;

                    END E;
               END T;

          BEGIN

               REC91 := (9, 9, "123456789");
               REC92 := REC91;
               REC93 := REC91;

               REC_OOPS := (4, 4, "OOPS");

          END PKG;

     BEGIN  -- (C)

          PKG.T.E (REC91, REC92, REC93);

     END;   -- (C)

     --------------------------------------------------

     RESULT;

END C95087C;
