#include "decimal-dummy.h"

#define dec32 std::decimal::decimal32
#define dec64 std::decimal::decimal64
#define dec128 std::decimal::decimal128

#include "pass_x.h"

void
pass_2_x (void)
{
DEBUG_INIT

#define T(NAME) testit##NAME ();

#ifndef SKIP_DECIMAL32
T(d32)
#endif
#ifndef SKIP_DECIMAL64
T(d64)
#endif
#ifndef SKIP_DECIMAL128
T(d128)
#endif

DEBUG_FINI

if (fails != 0)
  abort ();

#undef T
}
