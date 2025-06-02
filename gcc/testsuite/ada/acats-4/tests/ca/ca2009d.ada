-- CA2009D.ADA

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
-- CHECK THAT A GENERIC SUBPROGRAM SUBUNIT CAN BE SPECIFIED AND
-- INSTANTIATED.

-- BHS 8/01/84
-- JRK 5/24/85  CHANGED TO .ADA, SEE AI-00323.


WITH REPORT;
USE REPORT;
PROCEDURE CA2009D IS

     INT1 : INTEGER := 1;
     INT2 : INTEGER := 2;


     GENERIC
          TYPE ELEM IS PRIVATE;
          PCON1 : IN ELEM;
          PVAR1 : IN OUT ELEM;
     PROCEDURE PROC1;


     GENERIC
          TYPE OBJ IS PRIVATE;
          FCON1 : IN OBJ;
          FVAR1 : IN OUT OBJ;
     FUNCTION FUNC1 RETURN OBJ;


     PROCEDURE PROC1 IS SEPARATE;
     FUNCTION FUNC1 RETURN OBJ IS SEPARATE;


     PROCEDURE NI_PROC1 IS NEW PROC1 (INTEGER, 2, INT1);
     FUNCTION NI_FUNC1 IS NEW FUNC1 (INTEGER, 3, INT2);


BEGIN

     TEST ("CA2009D", "SPECIFICATION AND INSTANTIATION " &
                      "OF GENERIC SUBPROGRAM SUBUNITS");

     NI_PROC1;
     IF INT1 /= 2 THEN
          FAILED ("INCORRECT INSTANTIATION - NI_PROC1");
     END IF;


     IF NI_FUNC1 /= 3 THEN
          FAILED ("INCORRECT INSTANTIATION - NI_FUNC1");
     END IF;


     RESULT;

END CA2009D;


SEPARATE (CA2009D)
PROCEDURE PROC1 IS
BEGIN
     PVAR1 := PCON1;
END PROC1;


SEPARATE (CA2009D)
FUNCTION FUNC1 RETURN OBJ IS
BEGIN
     FVAR1 := FCON1;
     RETURN FVAR1;
END FUNC1;
