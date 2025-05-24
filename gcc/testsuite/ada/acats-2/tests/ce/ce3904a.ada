-- CE3904A.ADA

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
--     CHECK THAT THE LAST NONBLANK CHARACTER IN A FILE MAY BE READ BY
--     'GET' IN ENUMERATION_IO WITHOUT RAISING END_ERROR, AND THAT AFTER
--     THE LAST CHARACTER OF THE FILE HAS BEEN READ, ANY ATTEMPT TO READ
--     FURTHER CHARACTERS WILL RAISE END_ERROR.

-- HISTORY:
--     JET 08/19/88  CREATED ORIGINAL TEST.

WITH REPORT, TEXT_IO;  USE REPORT, TEXT_IO;
PROCEDURE CE3904A IS

     TYPE ENUM IS (THE, QUICK, BROWN, X);
     E : ENUM;

     PACKAGE EIO IS NEW ENUMERATION_IO(ENUM);
     USE EIO;

     F : FILE_TYPE;

     FILE_OK : BOOLEAN := FALSE;

BEGIN
     TEST ("CE3904A", "CHECK THAT THE LAST NONBLANK CHARACTER IN A " &
                      "FILE MAY BE READ BY 'GET' IN ENUMERATION_IO " &
                      "WITHOUT RAISING END_ERROR, AND THAT AFTER THE " &
                      "LAST CHARACTER OF THE FILE HAS BEEN READ, ANY " &
                      "ATTEMPT TO READ FURTHER CHARACTERS WILL RAISE " &
                      "END_ERROR");

     BEGIN
          CREATE(F, OUT_FILE, LEGAL_FILE_NAME);
          FILE_OK := TRUE;
     EXCEPTION
          WHEN OTHERS =>
               NOT_APPLICABLE("DATA FILE COULD NOT BE OPENED FOR " &
                              "WRITING");
     END;

     IF FILE_OK THEN
          BEGIN
               PUT(F, THE); NEW_LINE(F);
               PUT(F, QUICK); NEW_LINE(F);
               PUT(F, BROWN); NEW_LINE(F);
               PUT(F, X); NEW_LINE(F);
               CLOSE(F);
          EXCEPTION
               WHEN OTHERS =>
                    NOT_APPLICABLE("DATA FILE COULD NOT BE WRITTEN");
                    FILE_OK := FALSE;
          END;
     END IF;

     IF FILE_OK THEN
          BEGIN
               OPEN(F, IN_FILE, LEGAL_FILE_NAME);
               FOR I IN 0..3 LOOP
                    GET(F, E);
                    IF E /= ENUM'VAL(I) THEN
                         FAILED("INCORRECT VALUE READ -" &
                                INTEGER'IMAGE(I));
                    END IF;
               END LOOP;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED("UNEXPECTED EXCEPTION RAISED BEFORE END " &
                           "OF FILE");
                    FILE_OK := FALSE;
          END;
     END IF;

     IF FILE_OK THEN
          BEGIN
               GET(F, E);
               FAILED("NO EXCEPTION RAISED AFTER END OF FILE");
          EXCEPTION
               WHEN END_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED("INCORRECT EXCEPTION RAISED AFTER END OF " &
                           "FILE");
          END;

          BEGIN
               DELETE(F);
          EXCEPTION
               WHEN OTHERS =>
                    COMMENT("DATA FILE COULD NOT BE DELETED");
          END;
     END IF;

     RESULT;
END CE3904A;
