#include "decimal-dummy.h"

typedef std::decimal::decimal32 dec32;
typedef std::decimal::decimal64 dec64;
typedef std::decimal::decimal128 dec128;

#include "pass_x.h"

void
pass_6_x (void)
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
