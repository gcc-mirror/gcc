#include "compat-common.h"

#include "fp-struct-defs.h"
#include "fp-struct-check.h"
#include "fp-struct-test-by-value-x.h"

DEFS(cld, _Complex long double)
CHECKS(cld, _Complex long double)

TEST(Scld1, _Complex long double)
TEST(Scld2, _Complex long double)
TEST(Scld3, _Complex long double)
TEST(Scld4, _Complex long double)
TEST(Scld5, _Complex long double)
TEST(Scld6, _Complex long double)
TEST(Scld7, _Complex long double)
TEST(Scld8, _Complex long double)
TEST(Scld9, _Complex long double)
TEST(Scld10, _Complex long double)
TEST(Scld11, _Complex long double)
TEST(Scld12, _Complex long double)
TEST(Scld13, _Complex long double)
TEST(Scld14, _Complex long double)
TEST(Scld15, _Complex long double)
TEST(Scld16, _Complex long double)

#undef T

void
struct_by_value_18_x ()
{
DEBUG_INIT

#define T(TYPE, MTYPE) testit##TYPE ();

T(Scld1, _Complex long double)
T(Scld2, _Complex long double)
T(Scld3, _Complex long double)
T(Scld4, _Complex long double)
T(Scld5, _Complex long double)
T(Scld6, _Complex long double)
T(Scld7, _Complex long double)
T(Scld8, _Complex long double)
T(Scld9, _Complex long double)
T(Scld10, _Complex long double)
T(Scld11, _Complex long double)
T(Scld12, _Complex long double)
T(Scld13, _Complex long double)
T(Scld14, _Complex long double)
T(Scld15, _Complex long double)
T(Scld16, _Complex long double)

DEBUG_FINI

if (fails != 0)
  abort ();

#undef T
}
