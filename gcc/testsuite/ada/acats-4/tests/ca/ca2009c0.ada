-- CA2009C0M.ADA

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
--     CHECK THAT A GENERIC PACKAGE SUBUNIT CAN BE SPECIFIED AND
--     INSTANTIATED.  IN THIS TEST, THE SUBUNIT BODY IS IN A
--     SEPARATE FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST MUST RUN AND REPORT "PASSED" FOR ALL ADA 95 IMPLEMENTATIONS.

-- SEPARATE FILES ARE:
--     CA2009C0M  THE MAIN PROCEDURE.
--     CA2009C1   A SUBUNIT PACKAGE BODY (PKG1).

-- HISTORY:
--     BHS 08/01/84  CREATED ORIGINAL TEST.
--     BCB 01/05/88  MODIFIED HEADER.
--     EDS 08/04/98  REMOVE CONTROL Z AT END OF FILE.
--     RLB 09/13/99  UPDATED APPLICABILITY CRITERIA FOR ADA 95.
--     RLB 09/15/99  REMOVED JUNK COMMENT.

WITH REPORT;
USE REPORT;
PROCEDURE CA2009C0M IS

     INT1 : INTEGER := 1;

     SUBTYPE STR15 IS STRING (1..15);
     SVAR : STR15 := "ABCDEFGHIJKLMNO";

     GENERIC
          TYPE ITEM IS PRIVATE;
          CON1 : IN ITEM;
          VAR1 : IN OUT ITEM;
     PACKAGE PKG1 IS
     END PKG1;

     PACKAGE BODY PKG1 IS SEPARATE;

     PACKAGE NI_PKG1 IS NEW PKG1 (INTEGER, IDENT_INT(2), INT1);
     PACKAGE NS_PKG1 IS NEW PKG1 (STR15, IDENT_STR("REINSTANTIATION"),
                                  SVAR);

BEGIN

     TEST ("CA2009C", "SPECIFICATION AND INSTANTIATION " &
                      "OF GENERIC PACKAGE SUBUNITS " &
                      " -  SEPARATE FILES USED");

     IF INT1 /= 2 THEN
          FAILED ("INCORRECT INSTANTIATION - INTEGER");
     END IF;

     IF SVAR /= "REINSTANTIATION" THEN
          FAILED ("INCORRECT INSTANTIATION - STRING");
     END IF;


     RESULT;

END CA2009C0M;
