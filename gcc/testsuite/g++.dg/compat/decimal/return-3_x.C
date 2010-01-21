typedef float dec32 __attribute__((mode(SD)));
typedef float dec64 __attribute__((mode(DD)));
typedef float dec128 __attribute__((mode(TD)));

#include "return_x.h"

void
return_3_x (void)
{
DEBUG_INIT

#define T(NAME) testit##NAME ();

T(d32)
T(d64)
T(d128)

DEBUG_FINI

if (fails != 0)
  abort ();

#undef T
}
