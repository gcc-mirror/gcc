#include "compat-common.h"

#include "fp-struct-defs.h"
#include "fp-struct-check.h"
#include "fp-struct-test-by-value-x.h"

DEFS(ld, long double)
CHECKS(ld, long double)

TEST(Sld1, long double)
TEST(Sld2, long double)
TEST(Sld3, long double)
TEST(Sld4, long double)
TEST(Sld5, long double)
TEST(Sld6, long double)
TEST(Sld7, long double)
TEST(Sld8, long double)

#undef T

void
struct_by_value_7a_x ()
{
DEBUG_INIT

#define T(TYPE, MTYPE) testit##TYPE ();

T(Sld1, long double)
T(Sld2, long double)
T(Sld3, long double)
T(Sld4, long double)
T(Sld5, long double)
T(Sld6, long double)
T(Sld7, long double)
T(Sld8, long double)

DEBUG_FINI

if (fails != 0)
  abort ();

#undef T
}
