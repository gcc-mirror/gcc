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

TEST(Sd9, double)
TEST(Sd10, double)
TEST(Sd11, double)
TEST(Sd12, double)
TEST(Sd13, double)
TEST(Sd14, double)
TEST(Sd15, double)
TEST(Sd16, double)
