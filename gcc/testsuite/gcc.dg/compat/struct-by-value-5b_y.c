#include <stdarg.h>

#include "compat-common.h"

/* Turn off checking for variable arguments with -DSKIPVA.  */
#ifdef SKIPVA
const int test_va = 0;
#else
const int test_va = 1;
#endif

#include "fp-struct-defs.h"
#include "fp-struct-init.h"
#include "fp-struct-test-by-value-y.h"

DEFS(f,float)
INITS(f, float)

TEST(Sf9, float)
TEST(Sf10, float)
TEST(Sf11, float)
TEST(Sf12, float)
TEST(Sf13, float)
TEST(Sf14, float)
TEST(Sf15, float)
TEST(Sf16, float)
