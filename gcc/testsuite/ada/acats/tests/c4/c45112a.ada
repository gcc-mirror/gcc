-- C45112A.ADA

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
-- CHECK THAT THE BOUNDS OF THE RESULT OF A LOGICAL ARRAY OPERATION 
-- ARE THE BOUNDS OF THE LEFT OPERAND.

-- RJW 2/3/86

WITH REPORT; USE REPORT;

PROCEDURE C45112A IS 
     
     TYPE ARR IS ARRAY(INTEGER RANGE <>) OF BOOLEAN;
     A1 : ARR(IDENT_INT(3) .. IDENT_INT(4)) := (TRUE, FALSE);
     A2 : ARR(IDENT_INT(1) .. IDENT_INT(2)) := (TRUE, FALSE);
     SUBTYPE CARR IS ARR (IDENT_INT (A1'FIRST) .. IDENT_INT (A1'LAST));

     PROCEDURE CHECK (X : ARR; N1, N2 : STRING) IS
     BEGIN
          IF X'FIRST /= A1'FIRST OR X'LAST /= A1'LAST THEN
               FAILED ( "WRONG BOUNDS FOR " & N1 & " FOR " & N2 );
          END IF;
     END CHECK;
     
BEGIN

     TEST ( "C45112A", "CHECK THE BOUNDS OF THE RESULT OF LOGICAL " &
                       "ARRAY OPERATIONS" );

     BEGIN
          DECLARE
               AAND : CONSTANT ARR := A1 AND A2;
               AOR  : CONSTANT ARR := A1 OR A2;
               AXOR : CONSTANT ARR := A1 XOR A2;          
          BEGIN
               CHECK (AAND, "INITIALIZATION OF CONSTANT ARRAY ", 
                            "'AND'" );

               CHECK (AOR, "INITIALIZATION OF CONSTANT ARRAY ", 
                            "'OR'" );
     
               CHECK (AXOR, "INITIALIZATION OF CONSTANT ARRAY ", 
                            "'XOR'" );
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ( "CONSTRAINT_ERROR RAISED DURING " &
                        "INTIALIZATIONS" );
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED DURING " &
                        "INITIALIZATIONS" );
     END;
                              
     DECLARE
          PROCEDURE PROC (A : ARR; STR : STRING) IS
          BEGIN
               CHECK (A, "FORMAL PARAMETER FOR CONSTRAINED ARRAY", 
                      STR);
          END PROC;
     BEGIN
          PROC ((A1 AND A2), "'AND'" );
          PROC ((A1 OR A2), "'OR'" );
          PROC ((A1 XOR A2), "'XOR'" );
     EXCEPTION
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED DURING TEST FOR FORMAL " &
                        "PARAMETERS" );
     END;

     DECLARE
          FUNCTION FUNCAND RETURN ARR IS
          BEGIN
               RETURN A1 AND A2;
          END FUNCAND;

          FUNCTION FUNCOR RETURN ARR IS
          BEGIN
               RETURN A1 OR A2;
          END FUNCOR;
     
          FUNCTION FUNCXOR RETURN ARR IS
          BEGIN
               RETURN A1 XOR A2;
          END FUNCXOR;

     BEGIN
          CHECK (FUNCAND, "RETURN STATEMENT", "'AND'");
          CHECK (FUNCOR, "RETURN STATEMENT", "'OR'");
          CHECK (FUNCXOR, "RETURN STATEMENT", "'XOR'");
     
     EXCEPTION
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED DURING TEST FOR RETURN " &
                        "FROM FUNCTION" );
     END;

     BEGIN
          DECLARE
               GENERIC
                   X : IN ARR;
               PACKAGE PKG IS 
                    FUNCTION G RETURN ARR;
               END PKG;
     
               PACKAGE BODY PKG IS
                    FUNCTION G RETURN ARR IS
                    BEGIN
                         RETURN X;
                    END G;
               END PKG;
                         
               PACKAGE PAND IS NEW PKG(X => A1 AND A2);
               PACKAGE POR IS NEW PKG(X => A1 OR A2);
               PACKAGE PXOR IS NEW PKG(X => A1 XOR A2);
          BEGIN
               CHECK (PAND.G, "GENERIC FORMAL PARAMETER", "'AND'");
               CHECK (POR.G, "GENERIC FORMAL PARAMETER", "'OR'");
               CHECK (PXOR.G, "GENERIC FORMAL PARAMMETER", "'XOR'");
          END;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED DURING GENERIC " &
                        "INSTANTIATION" );
     END;

     DECLARE
          TYPE ACC IS ACCESS ARR;
          AC : ACC;
               
     BEGIN
          AC :=  NEW ARR'(A1 AND A2);
          CHECK (AC.ALL, "ALLOCATION", "'AND'");
          AC :=  NEW ARR'(A1 OR A2);
          CHECK (AC.ALL, "ALLOCATION", "'OR'");
          AC :=  NEW ARR'(A1 XOR A2);
          CHECK (AC.ALL, "ALLOCATION", "'XOR'");
     EXCEPTION
          WHEN OTHERS => 
               FAILED ( "EXCEPTION RAISED ON ALLOCATION" );
     END;

     BEGIN
          CHECK (CARR' (A1 AND A2), "QUALIFIED EXPRESSION", "'AND'");
          CHECK (CARR' (A1 OR A2), "QUALIFIED EXPRESSION", "'OR'");
          CHECK (CARR' (A1 XOR A2), "QUALIFIED EXPRESSION", "'XOR'");
     EXCEPTION
          WHEN OTHERS => 
               FAILED ( "EXCEPTION RAISED ON QUALIFIED EXPRESSION" );
     END;
               
     DECLARE
          TYPE REC IS
               RECORD
                    RCA : CARR;
               END RECORD;
          R1 : REC;

     BEGIN
          R1 := (RCA => (A1 AND A2));
          CHECK (R1.RCA, "AGGREGATE", "'AND'");
          R1 := (RCA => (A1 OR A2));
          CHECK (R1.RCA, "AGGREGATE", "'OR'");
          R1 := (RCA => (A1 XOR A2));
          CHECK (R1.RCA, "AGGREGATE", "'XOR'");
     EXCEPTION
          WHEN OTHERS => 
               FAILED ( "EXCEPTION RAISED ON AGGREGATE" );
     END;
                    
     BEGIN
          DECLARE
               TYPE RECDEF IS
                    RECORD
                         RCDF1 : CARR := A1 AND A2;
                         RCDF2 : CARR := A1 OR A2;
                         RCDF3 : CARR := A1 XOR A2;
                    END RECORD;
               RD : RECDEF;
          BEGIN
               CHECK (RD.RCDF1, "DEFAULT RECORD", "'AND'");
               CHECK (RD.RCDF2, "DEFAULT RECORD", "'OR'");
               CHECK (RD.RCDF3, "DEFAULT RECORD", "'XOR'");
          EXCEPTION
               WHEN OTHERS => 
                    FAILED ( "EXCEPTION RAISED ON DEFAULT RECORD" );
          END;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED DURING INITIALIZATION OF " &
                        "DEFAULT RECORD" );
     END;
               
     DECLARE 
          PROCEDURE PDEF (X : CARR := A1 AND A2; 
                          Y : CARR := A1 OR A2; 
                          Z : CARR := A1 XOR A2 ) IS
          BEGIN
               CHECK (X, "DEFAULT PARAMETER", "'AND'");
               CHECK (Y, "DEFAULT PARAMETER", "'OR'");
               CHECK (Z, "DEFAULT PARAMETER", "'XOR'");
          END PDEF;
     
     BEGIN
          PDEF;
     EXCEPTION
          WHEN OTHERS => 
               FAILED ( "EXCEPTION RAISED ON DEFAULT PARM" );
     END;

     RESULT;

END C45112A;
