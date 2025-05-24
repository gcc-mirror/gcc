-- CC3601A.ADA

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
-- CHECK THAT PREDEFINED OPERATORS MAY BE PASSED AS ACTUAL
-- GENERIC SUBPROGRAM PARAMETERS (CHECKS FOR "=" AND "/=" ARE IN
-- CC3601C).

-- R.WILLIAMS 10/9/86
-- JRL        11/15/95 Added unknown discriminant part to all formal
--                     private types.


WITH REPORT; USE REPORT;
PROCEDURE CC3601A IS

     GENERIC
          TYPE T (<>) IS PRIVATE;
          V, V1 : T;
          KIND : STRING;
          WITH FUNCTION F1 (X : IN T) RETURN T;
     PACKAGE GP1 IS
          R : BOOLEAN := F1 (V) = V1;
     END GP1;

     PACKAGE BODY GP1 IS
     BEGIN
          IF NOT (IDENT_BOOL(R)) THEN
               FAILED ( "INCORRECT VALUE FOR UNARY OP - " & KIND);
          END IF;
     END GP1;

     GENERIC
          TYPE T (<>) IS PRIVATE;
          V, V1, V2 : IN T;
          KIND : STRING;
          WITH FUNCTION F1 (P1 : IN T; P2 : IN T) RETURN T;
     PACKAGE GP2 IS
          R : BOOLEAN := V /= F1 (V1, V2);
     END GP2;

     PACKAGE BODY GP2 IS
     BEGIN
          IF IDENT_BOOL (R) THEN
               FAILED ( "INCORRECT VALUE FOR BINARY OP - " & KIND);
          END IF;
     END GP2;

     
     GENERIC
          TYPE T1 (<>) IS PRIVATE;
          TYPE T2 (<>) IS PRIVATE;
          V1 : T1;
          V2 : T2;
          KIND : STRING;
          WITH FUNCTION F1 (X : IN T1) RETURN T2;
     PACKAGE GP3 IS
          R : BOOLEAN := F1 (V1) = V2;
     END GP3;

     PACKAGE BODY GP3 IS
     BEGIN
          IF NOT (IDENT_BOOL(R)) THEN
               FAILED ( "INCORRECT VALUE FOR OP - " & KIND);
          END IF;
     END GP3;

BEGIN
     TEST ( "CC3601A", "CHECK THAT PREDEFINED OPERATORS MAY BE " &
                       "PASSED AS ACTUAL GENERIC SUBPROGRAM " &
                       "PARAMETERS" );


     BEGIN -- CHECKS WITH RELATIONAL OPERATORS AND LOGICAL OPERATORS AS
           -- ACTUAL PARAMETERS.

          FOR I1 IN BOOLEAN LOOP

               FOR I2 IN BOOLEAN LOOP
                    COMMENT ( "B1 = " & BOOLEAN'IMAGE (I1) & " AND " &
                              "B2 = " & BOOLEAN'IMAGE (I2) );
                    DECLARE
                         B1 : BOOLEAN := IDENT_BOOL (I1);
                         B2 : BOOLEAN := IDENT_BOOL (I2);

                         PACKAGE P1 IS 
                              NEW GP1 (BOOLEAN, NOT B2, B2, 
                                       """NOT"" - 1", "NOT");
                         PACKAGE P2 IS 
                              NEW GP2 (BOOLEAN, B1 OR B2, B1, B2, 
                                       "OR", "OR");
                         PACKAGE P3 IS  
                              NEW GP2 (BOOLEAN, B1 AND B2, B2, B1, 
                                       "AND", "AND");
                         PACKAGE P4 IS 
                              NEW GP2 (BOOLEAN, B1 /= B2, B1, B2, 
                                       "XOR", "XOR");
                         PACKAGE P5 IS  
                              NEW GP2 (BOOLEAN, B1 < B2, B1, B2, 
                                       "<", "<");
                         PACKAGE P6 IS 
                              NEW GP2 (BOOLEAN, B1 <= B2, B1, B2, 
                                       "<=", "<=");
                         PACKAGE P7 IS  
                              NEW GP2 (BOOLEAN, B1 > B2, B1, B2, 
                                       ">", ">");
                         PACKAGE P8 IS  
                              NEW GP2 (BOOLEAN, B1 >= B2, B1, B2, 
                                       ">=", ">=");

                         TYPE AB IS ARRAY (BOOLEAN RANGE <> ) 
                              OF BOOLEAN;
                         AB1 : AB (BOOLEAN) := (B1, B2);
                         AB2 : AB (BOOLEAN) := (B2, B1);
                         T : AB (B1 .. B2) := (B1 .. B2 => TRUE);
                         F : AB (B1 .. B2) := (B1 .. B2 => FALSE);
                         VB1 : AB (B1 .. B1) := (B1 => B2);
                         VB2 : AB (B2 .. B2) := (B2 => B1);

                         PACKAGE P9 IS 
                              NEW GP1 (AB, AB1, NOT AB1, 
                                       """NOT"" - 2", "NOT");
                         PACKAGE P10 IS 
                              NEW GP1 (AB, T, F, 
                                       """NOT"" - 3", "NOT");
                         PACKAGE P11 IS 
                              NEW GP1 (AB, VB2, (B2 => NOT B1), 
                                       """NOT"" - 4", "NOT");
                         PACKAGE P12 IS 
                              NEW GP2 (AB, AB1 AND AB2, AB1, AB2, 
                                       "AND", "AND");
                    BEGIN
                         NULL;
                    END;
               END LOOP;
          END LOOP;
     END;

     DECLARE -- CHECKS WITH ADDING AND MULTIPLYING OPERATORS, "**", 
             -- AND "ABS".
          
          PACKAGE P1 IS NEW GP1 (INTEGER, -4, -4, """+"" - 1", "+");

          PACKAGE P2 IS NEW GP1 (FLOAT, 4.0, 4.0, """+"" - 2", "+");

          PACKAGE P3 IS NEW GP1 (DURATION, -4.0, -4.0, """+"" - 3",
                                 "+");
          PACKAGE P4 IS NEW GP1 (INTEGER, -4, 4, """-"" - 1", "-");

          PACKAGE P5 IS NEW GP1 (FLOAT, 0.0, 0.0, """-"" - 2", "-");

          PACKAGE P6 IS NEW GP1 (DURATION, 1.0, -1.0, """-"" - 3", 
                                 "-");
          PACKAGE P7 IS NEW GP2 (INTEGER, 6, 1, 5, """+"" - 1", "+");

          PACKAGE P8 IS NEW GP2 (FLOAT, 6.0, 1.0, 5.0, """+"" - 2", 
                                 "+");
          PACKAGE P9 IS NEW GP2 (DURATION, 6.0, 1.0, 5.0, """+"" - 3",
                                 "+");
          PACKAGE P10 IS NEW GP2 (INTEGER, 1, 6, 5, """-"" - 1",
                                  "-" );
          PACKAGE P11 IS NEW GP2 (DURATION, 11.0, 6.0,-5.0, 
                                  """-"" - 2", "-");
          PACKAGE P12 IS NEW GP2 (FLOAT, 1.0, 6.0, 5.0, """-"" - 3",
                                  "-");

          SUBTYPE SUBINT IS INTEGER RANGE 0 .. 2;
          TYPE STR IS ARRAY (SUBINT RANGE <>) OF CHARACTER;
          VSTR : STR (0 .. 1) := "AB";

          PACKAGE P13 IS NEW GP2 (STR, VSTR (0 .. 0) & 
                                      VSTR (1 .. 1),
                                      VSTR (0 .. 0),  
                                      VSTR (1 .. 1), """&"" - 1", "&");

          PACKAGE P14 IS NEW GP2 (STR, VSTR (1 .. 1) & 
                                      VSTR (0 .. 0),
                                      VSTR (1 .. 1),  
                                      VSTR (0 .. 0), """&"" - 2", "&");
                                        
          PACKAGE P15 IS NEW GP2 (INTEGER, 0, -1, 0, """*"" - 1", "*");

          PACKAGE P16 IS NEW GP2 (FLOAT, 6.0, 3.0, 2.0, """*"" - 2",
                                  "*");
          PACKAGE P17 IS NEW GP2 (INTEGER, 0, 0, 6, """/"" - 1", "/");

          PACKAGE P18 IS NEW GP2 (FLOAT, 3.0, 6.0, 2.0, """/"" - 2",
                                  "/");
          PACKAGE P19 IS NEW GP2 (INTEGER, -1, -11, 5, "REM", "REM");

          PACKAGE P20 IS NEW GP2 (INTEGER, 4, -11, 5, "MOD", "MOD");
          
          PACKAGE P21 IS NEW GP1 (INTEGER, 5, 5, """ABS"" - 1", "ABS");

          PACKAGE P22 IS NEW GP1 (FLOAT, -5.0, 5.0, """ABS"" - 2", 
                                  "ABS");

          PACKAGE P23 IS NEW GP1 (DURATION, 0.0, 0.0, """ABS"" - 3", 
                                  "ABS");

          PACKAGE P24 IS NEW GP2 (INTEGER, 9, 3, 2, """**"" - 1", 
                                  "**");

          PACKAGE P25 IS NEW GP2 (INTEGER, 1, 5, 0, """**"" - 2",
                                  "**");

     BEGIN
          NULL;
     END;

     DECLARE -- CHECKS WITH ATTRIBUTES.
          
          TYPE WEEKDAY IS (MON, TUES, WED, THUR, FRI);

          PACKAGE P1 IS NEW GP1 (WEEKDAY, TUES, WED, "WEEKDAY'SUCC", 
                                 WEEKDAY'SUCC);

          PACKAGE P2 IS NEW GP1 (WEEKDAY, TUES, MON, "WEEKDAY'PRED", 
                                 WEEKDAY'PRED);
          
          PACKAGE P3 IS NEW GP3 (WEEKDAY, STRING, THUR, "THUR", 
                                 "WEEKDAY'IMAGE", WEEKDAY'IMAGE);

          PACKAGE P4 IS NEW GP3 (STRING, WEEKDAY, "FRI", FRI, 
                                 "WEEKDAY'VALUE", WEEKDAY'VALUE);
     BEGIN
          NULL;
     END;

     RESULT;
END CC3601A;
