#include <stdarg.h>

#include "compat-common.h"

#ifdef SKIP_VA
const int test_va = 0;
#else
const int test_va = 1;
#endif

#include "fp-struct-defs.h"
#include "fp-struct-init.h"
#include "fp-struct-test-by-value-y.h"

#ifndef SKIP_COMPLEX_INT
DEFS(cll,_Complex long long)
INITS(cll, _Complex long long)

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
