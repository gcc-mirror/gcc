#include "compat-common.h"

#include "fp-struct-defs.h"
#include "fp-struct-check.h"
#include "fp-struct-test-by-value-x.h"

#ifndef SKIP_COMPLEX
DEFS(cld, _Complex long double)
CHECKS(cld, _Complex long double)

TEST(Scld13, _Complex long double)
TEST(Scld14, _Complex long double)
TEST(Scld15, _Complex long double)
TEST(Scld16, _Complex long double)
#endif

#undef T

void
struct_by_value_18a_x ()
{
DEBUG_INIT

#define T(TYPE, MTYPE) testit##TYPE ();

#ifndef SKIP_COMPLEX
T(Scld13, _Complex long double)
T(Scld14, _Complex long double)
T(Scld15, _Complex long double)
T(Scld16, _Complex long double)
#endif

DEBUG_FINI

if (fails != 0)
  abort ();

#undef T
}
