typedef float dec32 __attribute__((mode(SD)));
typedef float dec64 __attribute__((mode(DD)));
typedef float dec128 __attribute__((mode(TD)));

#include "pass_x.h"

void
pass_5_x (void)
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
