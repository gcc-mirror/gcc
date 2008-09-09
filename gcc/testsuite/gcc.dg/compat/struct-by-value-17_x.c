#include "compat-common.h"

#include "fp-struct-defs.h"
#include "fp-struct-check.h"
#include "fp-struct-test-by-value-x.h"

#ifndef SKIP_COMPLEX
DEFS(cd, _Complex double)
CHECKS(cd, _Complex double)

TEST(Scd1, _Complex double)
TEST(Scd2, _Complex double)
TEST(Scd3, _Complex double)
TEST(Scd4, _Complex double)
TEST(Scd5, _Complex double)
TEST(Scd6, _Complex double)
TEST(Scd7, _Complex double)
TEST(Scd8, _Complex double)
TEST(Scd9, _Complex double)
TEST(Scd10, _Complex double)
TEST(Scd11, _Complex double)
TEST(Scd12, _Complex double)
#endif

#undef T

void
struct_by_value_17_x ()
{
DEBUG_INIT

#define T(TYPE, MTYPE) testit##TYPE ();

#ifndef SKIP_COMPLEX
T(Scd1, _Complex double)
T(Scd2, _Complex double)
T(Scd3, _Complex double)
T(Scd4, _Complex double)
T(Scd5, _Complex double)
T(Scd6, _Complex double)
T(Scd7, _Complex double)
T(Scd8, _Complex double)
T(Scd9, _Complex double)
T(Scd10, _Complex double)
T(Scd11, _Complex double)
T(Scd12, _Complex double)
#endif

DEBUG_FINI

if (fails != 0)
  abort ();

#undef T
}
