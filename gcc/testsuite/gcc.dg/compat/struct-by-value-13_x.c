#include "compat-common.h"

#include "fp-struct-defs.h"
#include "fp-struct-check.h"
#include "fp-struct-test-by-value-x.h"

#ifndef SKIP_COMPLEX_INT
DEFS(ci, _Complex int)
CHECKS(ci, _Complex int)

TEST(Sci1, _Complex int)
TEST(Sci2, _Complex int)
TEST(Sci3, _Complex int)
TEST(Sci4, _Complex int)
TEST(Sci5, _Complex int)
TEST(Sci6, _Complex int)
TEST(Sci7, _Complex int)
TEST(Sci8, _Complex int)
TEST(Sci9, _Complex int)
TEST(Sci10, _Complex int)
TEST(Sci11, _Complex int)
TEST(Sci12, _Complex int)
TEST(Sci13, _Complex int)
TEST(Sci14, _Complex int)
TEST(Sci15, _Complex int)
TEST(Sci16, _Complex int)
#endif

#undef T

void
struct_by_value_13_x ()
{
#ifndef SKIP_COMPLEX_INT
DEBUG_INIT

#define T(TYPE, MTYPE) testit##TYPE ();

T(Sci1, _Complex int)
T(Sci2, _Complex int)
T(Sci3, _Complex int)
T(Sci4, _Complex int)
T(Sci5, _Complex int)
T(Sci6, _Complex int)
T(Sci7, _Complex int)
T(Sci8, _Complex int)
T(Sci9, _Complex int)
T(Sci10, _Complex int)
T(Sci11, _Complex int)
T(Sci12, _Complex int)
T(Sci13, _Complex int)
T(Sci14, _Complex int)
T(Sci15, _Complex int)
T(Sci16, _Complex int)

DEBUG_FINI

if (fails != 0)
  abort ();

#undef T
#endif
}
