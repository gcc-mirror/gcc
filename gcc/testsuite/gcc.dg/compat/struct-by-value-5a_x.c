#include "compat-common.h"

#include "fp-struct-defs.h"
#include "fp-struct-check.h"
#include "fp-struct-test-by-value-x.h"

DEFS(f, float)
CHECKS(f, float)

TEST(Sf1, float)
TEST(Sf2, float)
TEST(Sf3, float)
TEST(Sf4, float)
TEST(Sf5, float)
TEST(Sf6, float)
TEST(Sf7, float)
TEST(Sf8, float)

#undef T

void
struct_by_value_5a_x ()
{
DEBUG_INIT

#define T(TYPE, MTYPE) testit##TYPE ();

T(Sf1, float)
T(Sf2, float)
T(Sf3, float)
T(Sf4, float)
T(Sf5, float)
T(Sf6, float)
T(Sf7, float)
T(Sf8, float)

DEBUG_FINI

if (fails != 0)
  abort ();

#undef T
}
