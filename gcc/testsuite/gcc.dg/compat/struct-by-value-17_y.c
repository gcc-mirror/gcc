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

DEFS(cd,_Complex double)
INITS(cd, _Complex double)

TEST(Scd1, _Complex double)
TEST(Scd2, _Complex double)
TEST(Scd3, _Complex double)
TEST(Scd4, _Complex double)
TEST(Scd5, _Complex double)
TEST(Scd6, _Complex double)
TEST(Scd7, _Complex double)
TEST(Scd8, _Complex double)
TEST(Scd9, _Complex double)
TEST(Scd10, _Complex double)
TEST(Scd11, _Complex double)
TEST(Scd12, _Complex double)
TEST(Scd13, _Complex double)
TEST(Scd14, _Complex double)
TEST(Scd15, _Complex double)
TEST(Scd16, _Complex double)
