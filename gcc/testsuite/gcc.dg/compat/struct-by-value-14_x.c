#include "compat-common.h"

#include "fp-struct-defs.h"
#include "fp-struct-check.h"
#include "fp-struct-test-by-value-x.h"

#ifndef SKIP_COMPLEX_INT
DEFS(cl, _Complex long)
CHECKS(cl, _Complex long)

TEST(Scl1, _Complex long)
TEST(Scl2, _Complex long)
TEST(Scl3, _Complex long)
TEST(Scl4, _Complex long)
TEST(Scl5, _Complex long)
TEST(Scl6, _Complex long)
TEST(Scl7, _Complex long)
TEST(Scl8, _Complex long)
TEST(Scl9, _Complex long)
TEST(Scl10, _Complex long)
TEST(Scl11, _Complex long)
TEST(Scl12, _Complex long)
TEST(Scl13, _Complex long)
TEST(Scl14, _Complex long)
TEST(Scl15, _Complex long)
TEST(Scl16, _Complex long)
#endif

#undef T

void
struct_by_value_14_x ()
{
#ifndef SKIP_COMPLEX_INT
DEBUG_INIT

#define T(TYPE, MTYPE) testit##TYPE ();

T(Scl1, _Complex long)
T(Scl2, _Complex long)
T(Scl3, _Complex long)
T(Scl4, _Complex long)
T(Scl5, _Complex long)
T(Scl6, _Complex long)
T(Scl7, _Complex long)
T(Scl8, _Complex long)
T(Scl9, _Complex long)
T(Scl10, _Complex long)
T(Scl11, _Complex long)
T(Scl12, _Complex long)
T(Scl13, _Complex long)
T(Scl14, _Complex long)
T(Scl15, _Complex long)
T(Scl16, _Complex long)

DEBUG_FINI

if (fails != 0)
  abort ();

#undef T
#endif
}
