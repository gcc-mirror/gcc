#include "compat-common.h"

#include "fp-struct-defs.h"
#include "fp-struct-check.h"
#include "fp-struct-test-by-value-x.h"

DEFS(d, double)
CHECKS(d, double)

TEST(Sd1, double)
TEST(Sd2, double)
TEST(Sd3, double)
TEST(Sd4, double)
TEST(Sd5, double)
TEST(Sd6, double)
TEST(Sd7, double)
TEST(Sd8, double)

#undef T

void
struct_by_value_6a_x ()
{
DEBUG_INIT

#define T(TYPE, MTYPE) testit##TYPE ();

T(Sd1, double)
T(Sd2, double)
T(Sd3, double)
T(Sd4, double)
T(Sd5, double)
T(Sd6, double)
T(Sd7, double)
T(Sd8, double)

DEBUG_FINI

if (fails != 0)
  abort ();

#undef T
}
