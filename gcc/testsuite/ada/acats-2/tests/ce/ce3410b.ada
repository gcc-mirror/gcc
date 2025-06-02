-- CE3410B.ADA

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
--     CHECK THAT SET_LINE RAISES CONSTRAINT_ERROR IF THE GIVEN
--     LINE NUMBER IS ZERO, OR NEGATIVE.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/22/82
--     JBG 01/27/83
--     JLH 08/31/87  ADDED CASE FOR COUNT'LAST.
--     PWN 10/27/95  REMOVED OUT OF RANGE STATIC VALUE CHECKS

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3410B IS

     FILE : FILE_TYPE;

BEGIN

     TEST ("CE3410B", "CHECK THAT SET_LINE RAISES CONSTRAINT_ERROR " &
                      "IF THE GIVEN LINE NUMBER IS ZERO, OR NEGATIVE");

     BEGIN
          SET_LINE (FILE, POSITIVE_COUNT(IDENT_INT(0)));
          FAILED ("CONSTRAINT_ERROR NOT RAISED FOR ZERO");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN STATUS_ERROR =>
               FAILED ("STATUS_ERROR INSTEAD OF CONSTRAINT_ERROR - 1");
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED FOR ZERO");
     END;

     BEGIN
          SET_LINE (FILE, POSITIVE_COUNT(IDENT_INT(-2)));
          FAILED ("CONSTRAINT_ERROR NOT RAISED FOR NEGATIVE NUMBER");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN STATUS_ERROR =>
               FAILED ("STATUS_ERROR INSTEAD OF CONSTRAINT_ERROR - 2");
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED FOR NEGATIVE " &
                       "NUMBER");
     END;

     RESULT;

END CE3410B;
