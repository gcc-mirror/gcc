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

DEFS(ld, long double)
INITS(ld, long double)

TEST(Sld9, long double)
TEST(Sld10, long double)
TEST(Sld11, long double)
TEST(Sld12, long double)
TEST(Sld13, long double)
TEST(Sld14, long double)
TEST(Sld15, long double)
TEST(Sld16, long double)
