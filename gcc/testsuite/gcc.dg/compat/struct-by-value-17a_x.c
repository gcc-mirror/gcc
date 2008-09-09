#include "compat-common.h"

#include "fp-struct-defs.h"
#include "fp-struct-check.h"
#include "fp-struct-test-by-value-x.h"

#ifndef SKIP_COMPLEX
DEFS(cd, _Complex double)
CHECKS(cd, _Complex double)

TEST(Scd13, _Complex double)
TEST(Scd14, _Complex double)
TEST(Scd15, _Complex double)
TEST(Scd16, _Complex double)
#endif

#undef T

void
struct_by_value_17a_x ()
{
DEBUG_INIT

#define T(TYPE, MTYPE) testit##TYPE ();

#ifndef SKIP_COMPLEX
T(Scd13, _Complex double)
T(Scd14, _Complex double)
T(Scd15, _Complex double)
T(Scd16, _Complex double)
#endif

DEBUG_FINI

if (fails != 0)
  abort ();

#undef T
}
