-- CD2B16A.ADA

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
--     IF A COLLECTION SIZE CLAUSE IS GIVEN FOR A PARENT ACCESS TYPE,
--     THEN THE DERIVED TYPE HAS THE SAME COLLECTION SIZE, WHETHER THE
--     DERIVED TYPE IS DECLARED BEFORE OR AFTER THE PARENT COLLECTION
--     SIZE SPECIFICATION.

-- HISTORY:
--     DHH 09/29/87 CREATED ORIGINAL TEST.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

WITH SYSTEM;
WITH REPORT; USE REPORT;
PROCEDURE CD2B16A IS
BEGIN
     TEST ("CD2B16A", "IF A COLLECTION SIZE IS GIVEN FOR A " &
                      "PARENT ACCESS TYPE, THEN THE DERIVED TYPE HAS " &
                      "THE SAME COLLECTION SIZE, WHETHER THE " &
                      "DERIVED TYPE IS DECLARED BEFORE OR AFTER " &
                      "THE PARENT COLLECTION SIZE SPECIFICATION");

          DECLARE

               COLLECTION_SIZE : CONSTANT :=128;
               TYPE V IS ARRAY(1..4) OF INTEGER;

               TYPE CELL IS
                    RECORD
                         VALUE : V;
                    END RECORD;

               TYPE LINK IS ACCESS CELL;
               TYPE NEWLINK1 IS NEW LINK;

               FOR LINK'STORAGE_SIZE USE
                                     COLLECTION_SIZE;

               TYPE NEWLINK2 IS NEW LINK;

          BEGIN    -- ACTIVE DECLARE

               IF LINK'STORAGE_SIZE < COLLECTION_SIZE THEN
                    FAILED("STORAGE_SIZE SMALLER THAN STORAGE_SIZE " &
                           "SPECIFIED WAS ALLOCATED");
               END IF;

               IF LINK'STORAGE_SIZE /= NEWLINK1'STORAGE_SIZE THEN
                    FAILED("STORAGE_SIZE OF THE FIRST DERIVED TYPE" &
                           "IS NOT THE SAME SIZE AS THAT OF THE " &
                           "PARENT");
               END IF;

               IF LINK'STORAGE_SIZE /= NEWLINK2'STORAGE_SIZE THEN
                    FAILED("STORAGE_SIZE OF THE SECOND DERIVED TYPE" &
                           "IS NOT THE SAME SIZE AS THAT OF THE " &
                           "PARENT");
               END IF;

          END;    --ACTIVE DECLARE

     RESULT;
END CD2B16A;
