-- C36204C.ADA

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
--     CHECK THAT THE 'RANGE ATTRIBUTE CAN BE USED TO DECLARE OBJECTS
--     AND IN A SUBTYPE AND TYPE DECLARATION.

-- HISTORY:
--     LB  08/13/86  CREATED ORIGINAL TEST.
--     BCB 08/18/87  CHANGED HEADER TO STANDARD HEADER FORMAT.
--                   REARRANGED STATEMENTS SO TEST IS CALLED FIRST.
--                   ELIMINATED DEAD VARIABLE OPTIMIZATION.  CHECKED
--                   RANGE VALUES FOR A SMALL INTEGER.

WITH REPORT; USE REPORT;
PROCEDURE  C36204C  IS

BEGIN
     TEST("C36204C","USING 'RANGE TO DECLARE OBJECTS AND " &
                    "IN A SUBTYPE AND TYPE DECLARATION " &
                    "RETURNS THE CORRECT VALUES.");

     DECLARE

          ARR : ARRAY(IDENT_INT(4) .. IDENT_INT(10)) OF INTEGER;
          OBJ1 : ARRAY(ARR'RANGE) OF BOOLEAN;

          SUBTYPE SMALL_INT IS INTEGER RANGE ARR'RANGE ;
          SML : SMALL_INT;

          TYPE OTHER_ARR IS ARRAY(ARR'RANGE) OF CHARACTER;
          OBJ2 : OTHER_ARR;

          TYPE ARR_TYPE IS ARRAY(INTEGER RANGE IDENT_INT(1) ..
                                 IDENT_INT(10)) OF INTEGER;
          TYPE ARR_PTR IS ACCESS ARR_TYPE;
          PTR : ARR_PTR := NEW ARR_TYPE'(ARR_TYPE'RANGE => 0);

          FUNCTION F RETURN ARR_TYPE IS
               AR : ARR_TYPE := (ARR_TYPE'RANGE => 0);
               BEGIN
                    RETURN AR;
               END F;

          BEGIN
               BEGIN
                    IF OBJ1'FIRST /= IDENT_INT(4)  THEN
                         FAILED("INCORRECT RANGE VALUE FOR AN OBJECT " &
                                "DECLARATION 1");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED("EXCEPTION RAISED WHEN CHECKING " &
                                "OBJECT DECLARATION 1");
               END;

               BEGIN
                    IF OBJ1'LAST /= IDENT_INT(10)  THEN
                         FAILED("INCORRECT RANGE VALUE FOR AN OBJECT " &
                                "DECLARATION 2");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED("EXCEPTION RAISED WHEN CHECKING " &
                                "OBJECT DECLARATION 2");
               END;

               BEGIN
                    IF SMALL_INT'FIRST /= 4 THEN
                         FAILED("INCORRECT RANGE VALUE FOR A SMALL " &
                                "INTEGER DECLARATION 1");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED("EXCEPTION RAISED WHEN CHECKING SMALL" &
                                " INTEGER DECLARATION 1");
               END;

               BEGIN
                    IF SMALL_INT'LAST /= 10 THEN
                         FAILED("INCORRECT RANGE VALUE FOR A SMALL " &
                                "INTEGER DECLARATION 2");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED("EXCEPTION RAISED WHEN CHECKING SMALL" &
                                " INTEGER DECLARATION 2");
               END;

               BEGIN
                    SML := IDENT_INT(3) ;
                    IF SML = 3 THEN
                         COMMENT("VARIABLE SML OPTIMIZED VALUE 1");
                    END IF;
                    FAILED("NO EXCEPTION RAISED FOR OUT-OF RANGE " &
                           "VALUE 1");
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED("WRONG EXCEPTION RAISED FOR OUT-OF " &
                                "RANGE VALUE 1");
               END;

               BEGIN
                    SML := IDENT_INT(11) ;
                    IF SML = 11 THEN
                         COMMENT("VARIABLE SML OPTIMIZED VALUE 2");
                    END IF;
                    FAILED("NO EXCEPTION RAISED FOR OUT-OF RANGE " &
                           "VALUE 2");
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED("WRONG EXCEPTION RAISED FOR OUT-OF " &
                                "RANGE VALUE 2");
               END;

               BEGIN
                    IF OBJ2'FIRST /= IDENT_INT(4)  THEN
                         FAILED("INCORRECT RANGE VALUE FOR A TYPE " &
                                "DECLARATION 1");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED("EXCEPTION RAISED WHEN CHECKING A " &
                                "TYPE DECLARATION 1");
               END;

               BEGIN
                    IF OBJ2'LAST /= IDENT_INT(10)  THEN
                         FAILED("INCORRECT RANGE VALUE FOR A TYPE " &
                                "DECLARATION 2");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED("EXCEPTION RAISED WHEN CHECKING A " &
                                "TYPE DECLARATION 2");
               END;

               BEGIN
                    IF PTR'FIRST /= IDENT_INT(1)  THEN
                         FAILED("INCORRECT RANGE VALUE FOR AN ACCESS " &
                                "TYPE DECLARATION 1");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED("EXCEPTION RAISED WHEN CHECKING AN " &
                                "ACCESS TYPE DECLARATION 1");
               END;

               BEGIN
                    IF PTR'LAST /= IDENT_INT(10)  THEN
                         FAILED("INCORRECT RANGE VALUE FOR AN ACCESS " &
                                "TYPE DECLARATION 2");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED("EXCEPTION RAISED WHEN CHECKING AN " &
                                "ACCESS TYPE DECLARATION 2");
               END;

               DECLARE
                    OBJ_F1 : INTEGER RANGE F'RANGE ;
               BEGIN
                    OBJ_F1 := IDENT_INT(0) ;
                    IF OBJ_F1 = 0 THEN
                         COMMENT("VARIABLE OBJ_F1 OPTIMIZED VALUE 1");
                    END IF;
                    FAILED("NO EXCEPTION RAISED FOR OUT-OF RANGE " &
                           "VALUE 3");
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED("WRONG EXCEPTION RAISED FOR OUT-OF " &
                                "RANGE VALUE 3");
               END;

               DECLARE
                    OBJ_F2 : INTEGER RANGE F'RANGE ;
               BEGIN
                    OBJ_F2 := IDENT_INT(11) ;
                    IF OBJ_F2 = 11 THEN
                         COMMENT("VARIABLE OBJ_F2 OPTIMIZED VALUE 1");
                    END IF;
                    FAILED("NO EXCEPTION RAISED FOR OUT-OF RANGE " &
                           "VALUE 4");
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED("WRONG EXCEPTION RAISED FOR OUT-OF " &
                                "RANGE VALUE 4");
               END;
          END;
     RESULT;

END C36204C;
