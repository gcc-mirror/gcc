#include "compat-common.h"

#include "fp-struct-defs.h"
#include "fp-struct-check.h"
#include "fp-struct-test-by-value-x.h"

DEFS(ld, long double)
CHECKS(ld, long double)

TEST(Sld9, long double)
TEST(Sld10, long double)
TEST(Sld11, long double)
TEST(Sld12, long double)
TEST(Sld13, long double)
TEST(Sld14, long double)
TEST(Sld15, long double)
TEST(Sld16, long double)

#undef T

void
struct_by_value_7b_x ()
{
DEBUG_INIT

#define T(TYPE, MTYPE) testit##TYPE ();

T(Sld9, long double)
T(Sld10, long double)
T(Sld11, long double)
T(Sld12, long double)
T(Sld13, long double)
T(Sld14, long double)
T(Sld15, long double)
T(Sld16, long double)

DEBUG_FINI

if (fails != 0)
  abort ();

#undef T
}
