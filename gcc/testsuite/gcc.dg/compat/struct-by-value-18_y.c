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

DEFS(cld,_Complex long double)
INITS(cld, _Complex long double)

TEST(Scld1, _Complex long double)
TEST(Scld2, _Complex long double)
TEST(Scld3, _Complex long double)
TEST(Scld4, _Complex long double)
TEST(Scld5, _Complex long double)
TEST(Scld6, _Complex long double)
TEST(Scld7, _Complex long double)
TEST(Scld8, _Complex long double)
TEST(Scld9, _Complex long double)
TEST(Scld10, _Complex long double)
TEST(Scld11, _Complex long double)
TEST(Scld12, _Complex long double)
TEST(Scld13, _Complex long double)
TEST(Scld14, _Complex long double)
TEST(Scld15, _Complex long double)
TEST(Scld16, _Complex long double)
