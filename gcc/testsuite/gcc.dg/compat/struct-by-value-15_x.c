#include "compat-common.h"

#include "fp-struct-defs.h"
#include "fp-struct-check.h"
#include "fp-struct-test-by-value-x.h"

#ifndef SKIP_COMPLEX_INT
DEFS(cll, _Complex long long)
CHECKS(cll, _Complex long long)

TEST(Scll1, _Complex long long)
TEST(Scll2, _Complex long long)
TEST(Scll3, _Complex long long)
TEST(Scll4, _Complex long long)
TEST(Scll5, _Complex long long)
TEST(Scll6, _Complex long long)
TEST(Scll7, _Complex long long)
TEST(Scll8, _Complex long long)
TEST(Scll9, _Complex long long)
TEST(Scll10, _Complex long long)
TEST(Scll11, _Complex long long)
TEST(Scll12, _Complex long long)
TEST(Scll13, _Complex long long)
TEST(Scll14, _Complex long long)
TEST(Scll15, _Complex long long)
TEST(Scll16, _Complex long long)
#endif

#undef T

void
struct_by_value_15_x ()
{
#ifndef SKIP_COMPLEX_INT
DEBUG_INIT

#define T(TYPE, MTYPE) testit##TYPE ();

T(Scll1, _Complex long long)
T(Scll2, _Complex long long)
T(Scll3, _Complex long long)
T(Scll4, _Complex long long)
T(Scll5, _Complex long long)
T(Scll6, _Complex long long)
T(Scll7, _Complex long long)
T(Scll8, _Complex long long)
T(Scll9, _Complex long long)
T(Scll10, _Complex long long)
T(Scll11, _Complex long long)
T(Scll12, _Complex long long)
T(Scll13, _Complex long long)
T(Scll14, _Complex long long)
T(Scll15, _Complex long long)
T(Scll16, _Complex long long)

DEBUG_FINI

if (fails != 0)
  abort ();

#undef T
#endif
}
