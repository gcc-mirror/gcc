#include "compat-common.h"

#include "fp-struct-defs.h"
#include "fp-struct-check.h"
#include "fp-struct-test-by-value-x.h"

DEFS(f, float)
CHECKS(f, float)

TEST(Sf9, float)
TEST(Sf10, float)
TEST(Sf11, float)
TEST(Sf12, float)
TEST(Sf13, float)
TEST(Sf14, float)
TEST(Sf15, float)
TEST(Sf16, float)

#undef T

void
struct_by_value_5b_x ()
{
DEBUG_INIT

#define T(TYPE, MTYPE) testit##TYPE ();

T(Sf9, float)
T(Sf10, float)
T(Sf11, float)
T(Sf12, float)
T(Sf13, float)
T(Sf14, float)
T(Sf15, float)
T(Sf16, float)

DEBUG_FINI

if (fails != 0)
  abort ();

#undef T
}
