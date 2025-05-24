/*
-- CXB30230.C
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--*
--
-- FUNCTION NAME: CXB30230
--
-- FUNCTION DESCRIPTION:
--      This C function converts a value of type variant_t into a string
--      value representing that value.
--
-- INPUTS:
--      This function requires that two parameters be passed to it, a pointer
--      to the value of type variant_t, and a pointer to a buffer for the
--      string result.
--
-- OUTPUTS:
--      The resulting string will be written as a nul-terminated string to
--      the buffer. Note that the routine does not check that the string will
--      fit in the buffer!
--
-- OBJECTIVE
--      See CXB30231.AM.
--
-- TEST DESCRIPTION
--      See CXB30231.AM.
--
-- TEST FILES:
--      This test consists of the following files:
--      -> CXB30230.C
--         CXB30231.AM
--
-- CHANGE HISTORY:
--    06 Sep 2015 BJM Created function.
--    25 Nov 2015 RLB Changed naming consistent with ACATS.
--    09 Dec 2015 RLB Removed void pointer case.
--
--!*/
#include <stdio.h>

typedef enum { INTEGER = 0,
               CHARACTER = 1,
               STRING = 2,
               REAL = 3} data_kind_t;

typedef struct
{
  data_kind_t type;

  union {
     int integer;
     char character;
     char *string;
     float real;
  } x;
} variant_t;


void CXB30230 (const variant_t *var, char *output)
{
  switch (var->type)
    {
      case INTEGER:
        sprintf(output, "Type: Integer, Val: %d", var->x.integer);
        break;

      case CHARACTER:
        sprintf(output, "Type: Character, Val: '%c'", var->x.character);
        break;

      case STRING:
        sprintf(output, "Type: String, Val: %s", var->x.string);
        break;

      case REAL:
        sprintf(output, "Type: Real, Val: %.2f", var->x.real);
        break;

      default:
        sprintf(output, "Type %d, ERR: Invalid!", var->type);

    }
}
