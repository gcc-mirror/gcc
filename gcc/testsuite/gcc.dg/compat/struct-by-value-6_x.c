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
TEST(Sd9, double)
TEST(Sd10, double)
TEST(Sd11, double)
TEST(Sd12, double)
TEST(Sd13, double)
TEST(Sd14, double)
TEST(Sd15, double)
TEST(Sd16, double)

#undef T

void
struct_by_value_6_x ()
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
T(Sd9, double)
T(Sd10, double)
T(Sd11, double)
T(Sd12, double)
T(Sd13, double)
T(Sd14, double)
T(Sd15, double)
T(Sd16, double)

DEBUG_FINI

if (fails != 0)
  abort ();

#undef T
}
