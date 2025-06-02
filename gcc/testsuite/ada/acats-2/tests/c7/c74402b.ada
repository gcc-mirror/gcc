-- C74402B.ADA

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
-- CHECK THAT INITIALIZATION OF IN PARAMETERS THAT ARE OF
-- LIMITED PRIVATE TYPE IS PERMITTED.
-- (SEE ALSO 6.4.2/T1 FOR TESTS OF OTHER LIMITED TYPES.)

-- DAS  1/21/81
-- ABW  6/30/82
-- BHS  7/10/84

WITH REPORT;
PROCEDURE C74402B IS

     USE REPORT;

BEGIN

     TEST( "C74402B" , "CHECK THAT INITIALIZATION OF IN PARAMETERS " &
                       "OF LIMITED PRIVATE TYPE IS PERMITTED" );

     DECLARE

          PACKAGE PKG IS

               TYPE LPTYPE IS LIMITED PRIVATE;
               CLP : CONSTANT LPTYPE;
               XLP : CONSTANT LPTYPE;
               FUNCTION EQCLP (L : IN LPTYPE) RETURN BOOLEAN;
               FUNCTION EQXLP (L : IN LPTYPE) RETURN BOOLEAN;

          PRIVATE

               TYPE LPTYPE IS NEW INTEGER RANGE 0..127;
               CLP : CONSTANT LPTYPE := 127;
               XLP : CONSTANT LPTYPE := 0;

          END;

          PACKAGE BODY PKG IS

               FUNCTION EQCLP (L : IN LPTYPE) RETURN BOOLEAN IS
               BEGIN
                    RETURN (L = CLP);
               END EQCLP;

               FUNCTION EQXLP (L : IN LPTYPE) RETURN BOOLEAN IS
               BEGIN
                    RETURN (L = XLP);
               END EQXLP;

          END PKG;

          USE PKG;

          PROCEDURE PROC1 (Y : IN LPTYPE := CLP) IS
          BEGIN
               IF (EQCLP (Y)) THEN
                    FAILED( "LIMITED PRIVATE NOT PASSED, " &
                            "DEFAULT CLP EMPLOYED" );
               ELSIF (NOT EQXLP (Y)) THEN
                    FAILED( "NO LIMITED PRIVATE FOUND" );
               END IF;
          END PROC1;

          PROCEDURE PROC2 (Y : IN LPTYPE := CLP) IS
          BEGIN
               IF (NOT EQCLP(Y)) THEN
                    FAILED( "DEFAULT NOT EMPLOYED" );
               END IF;
          END PROC2;

     BEGIN

          PROC1(XLP);
          PROC2;

     END;

     RESULT;

END C74402B;
