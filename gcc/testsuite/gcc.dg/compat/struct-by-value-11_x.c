#include "compat-common.h"

#include "fp-struct-defs.h"
#include "fp-struct-check.h"
#include "fp-struct-test-by-value-x.h"

#ifndef SKIP_COMPLEX_INT
DEFS(cc, _Complex char)
CHECKS(cc, _Complex char)

TEST(Scc1, _Complex char)
TEST(Scc2, _Complex char)
TEST(Scc3, _Complex char)
TEST(Scc4, _Complex char)
TEST(Scc5, _Complex char)
TEST(Scc6, _Complex char)
TEST(Scc7, _Complex char)
TEST(Scc8, _Complex char)
TEST(Scc9, _Complex char)
TEST(Scc10, _Complex char)
TEST(Scc11, _Complex char)
TEST(Scc12, _Complex char)
TEST(Scc13, _Complex char)
TEST(Scc14, _Complex char)
TEST(Scc15, _Complex char)
TEST(Scc16, _Complex char)
#endif

#undef T

void
struct_by_value_11_x ()
{
#ifndef SKIP_COMPLEX_INT
DEBUG_INIT

#define T(TYPE, MTYPE) testit##TYPE ();

T(Scc1, _Complex char)
T(Scc2, _Complex char)
T(Scc3, _Complex char)
T(Scc4, _Complex char)
T(Scc5, _Complex char)
T(Scc6, _Complex char)
T(Scc7, _Complex char)
T(Scc8, _Complex char)
T(Scc9, _Complex char)
T(Scc10, _Complex char)
T(Scc11, _Complex char)
T(Scc12, _Complex char)
T(Scc13, _Complex char)
T(Scc14, _Complex char)
T(Scc15, _Complex char)
T(Scc16, _Complex char)

DEBUG_FINI

if (fails != 0)
  abort ();

#undef T
#endif
}
