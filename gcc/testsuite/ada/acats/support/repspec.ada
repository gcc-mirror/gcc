-- REPSPEC.ADA
--
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
--
-- PURPOSE:
--      THIS REPORT PACKAGE PROVIDES THE MECHANISM FOR REPORTING THE
--      PASS/FAIL/NOT-APPLICABLE RESULTS OF EXECUTABLE (CLASSES A, C,
--      D, E, AND L) TESTS.

--      IT ALSO PROVIDES THE MECHANISM FOR GUARANTEEING THAT CERTAIN
--      VALUES BECOME DYNAMIC (NOT KNOWN AT COMPILE-TIME).

-- HISTORY:
--      JRK 12/13/79
--      JRK 06/10/80
--      JRK 08/06/81
--      JRK 10/27/82
--      JRK 06/01/84
--      PWB 07/30/87  ADDED PROCEDURE SPECIAL_ACTION.
--      TBN 08/20/87  ADDED FUNCTION LEGAL_FILE_NAME.
--      BCB 05/17/90  ADDED FUNCTION TIME_STAMP.
--      WMC 01/24/94  INCREASED RANGE OF TYPE FILE_NUM FROM 1..3 TO 1..5.
--      KAS 06/19/95  ADDED FUNCTION IDENT_WIDE_CHAR.
--      KAS 06/19/95  ADDED FUNCTION IDENT_WIDE_STR.

PACKAGE REPORT IS

     SUBTYPE FILE_NUM IS INTEGER RANGE 1..5;

  -- THE REPORT ROUTINES.

     PROCEDURE TEST           -- THIS ROUTINE MUST BE INVOKED AT THE
                              -- START OF A TEST, BEFORE ANY OF THE
                              -- OTHER REPORT ROUTINES ARE INVOKED.
                              -- IT SAVES THE TEST NAME AND OUTPUTS THE
                              -- NAME AND DESCRIPTION.
        ( NAME : STRING;      -- TEST NAME, E.G., "C23001A-AB".
          DESCR : STRING      -- BRIEF DESCRIPTION OF TEST, E.G.,
                              -- "UPPER/LOWER CASE EQUIVALENCE IN " &
                              -- "IDENTIFIERS".
        );

     PROCEDURE FAILED         -- OUTPUT A FAILURE MESSAGE.  SHOULD BE
                              -- INVOKED SEPARATELY TO REPORT THE
                              -- FAILURE OF EACH SUBTEST WITHIN A TEST.
        ( DESCR : STRING      -- BRIEF DESCRIPTION OF WHAT FAILED.
                              -- SHOULD BE PHRASED AS:
                              -- "(FAILED BECAUSE) ...REASON...".
        );

     PROCEDURE NOT_APPLICABLE -- OUTPUT A NOT-APPLICABLE MESSAGE.
                              -- SHOULD BE INVOKED SEPARATELY TO REPORT
                              -- THE NON-APPLICABILITY OF EACH SUBTEST
                              -- WITHIN A TEST.
        ( DESCR : STRING      -- BRIEF DESCRIPTION OF WHAT IS
                              -- NOT-APPLICABLE. SHOULD BE PHRASED AS:
                              -- "(NOT-APPLICABLE BECAUSE)...REASON...".
        );

     PROCEDURE SPECIAL_ACTION -- OUTPUT A MESSAGE DESCRIBING SPECIAL
                              -- ACTIONS TO BE TAKEN.
                              -- SHOULD BE INVOKED SEPARATELY TO GIVE
                              -- EACH SPECIAL ACTION.
        ( DESCR : STRING      -- BRIEF DESCRIPTION OF ACTION TO BE
                              -- TAKEN.
        );

     PROCEDURE COMMENT        -- OUTPUT A COMMENT MESSAGE.
        ( DESCR : STRING      -- THE MESSAGE.
        );

     PROCEDURE RESULT;        -- THIS ROUTINE MUST BE INVOKED AT THE
                              -- END OF A TEST.  IT OUTPUTS A MESSAGE
                              -- INDICATING WHETHER THE TEST AS A
                              -- WHOLE HAS PASSED, FAILED, IS
                              -- NOT-APPLICABLE, OR HAS TENTATIVELY
                              -- PASSED PENDING SPECIAL ACTIONS.

  -- THE DYNAMIC VALUE ROUTINES.

     -- EVEN WITH STATIC ARGUMENTS, THESE FUNCTIONS WILL HAVE DYNAMIC
     -- RESULTS.

     FUNCTION IDENT_INT       -- AN IDENTITY FUNCTION FOR TYPE INTEGER.
        ( X : INTEGER         -- THE ARGUMENT.
        ) RETURN INTEGER;     -- X.

     FUNCTION IDENT_CHAR      -- AN IDENTITY FUNCTION FOR TYPE
                              -- CHARACTER.
        ( X : CHARACTER       -- THE ARGUMENT.
        ) RETURN CHARACTER;   -- X.

     FUNCTION IDENT_WIDE_CHAR -- AN IDENTITY FUNCTION FOR TYPE
                              -- WIDE_CHARACTER.
        ( X : WIDE_CHARACTER  -- THE ARGUMENT.
        ) RETURN WIDE_CHARACTER; -- X.

     FUNCTION IDENT_BOOL      -- AN IDENTITY FUNCTION FOR TYPE BOOLEAN.
        ( X : BOOLEAN         -- THE ARGUMENT.
        ) RETURN BOOLEAN;     -- X.

     FUNCTION IDENT_STR       -- AN IDENTITY FUNCTION FOR TYPE STRING.
        ( X : STRING          -- THE ARGUMENT.
        ) RETURN STRING;      -- X.

     FUNCTION IDENT_WIDE_STR  -- AN IDENTITY FUNCTION FOR TYPE WIDE_STRING.
        ( X : WIDE_STRING     -- THE ARGUMENT.
        ) RETURN WIDE_STRING; -- X.

     FUNCTION EQUAL           -- A RECURSIVE EQUALITY FUNCTION FOR TYPE
                              -- INTEGER.
        ( X, Y : INTEGER      -- THE ARGUMENTS.
        ) RETURN BOOLEAN;     -- X = Y.

-- OTHER UTILITY ROUTINES.

     FUNCTION LEGAL_FILE_NAME -- A FUNCTION TO GENERATE LEGAL EXTERNAL
                              -- FILE NAMES.
        ( X : FILE_NUM := 1;  -- DETERMINES FIRST CHARACTER OF NAME.
          NAM : STRING := ""  -- DETERMINES REST OF NAME.
        ) RETURN STRING;      -- THE GENERATED NAME.

     FUNCTION TIME_STAMP      -- A FUNCTION TO GENERATE THE TIME AND
                              -- DATE TO PLACE IN THE OUTPUT OF AN ACVC
                              -- TEST.
          RETURN STRING;      -- THE TIME AND DATE.

END REPORT;
