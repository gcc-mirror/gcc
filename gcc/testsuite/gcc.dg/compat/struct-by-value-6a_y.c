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

DEFS(d, double)
INITS(d, double)

TEST(Sd1, double)
TEST(Sd2, double)
TEST(Sd3, double)
TEST(Sd4, double)
TEST(Sd5, double)
TEST(Sd6, double)
TEST(Sd7, double)
TEST(Sd8, double)
