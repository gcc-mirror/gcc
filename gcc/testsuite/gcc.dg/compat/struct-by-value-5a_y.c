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

DEFS(f,float)
INITS(f, float)

TEST(Sf1, float)
TEST(Sf2, float)
TEST(Sf3, float)
TEST(Sf4, float)
TEST(Sf5, float)
TEST(Sf6, float)
TEST(Sf7, float)
TEST(Sf8, float)
