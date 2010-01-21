#include "decimal-dummy.h"

typedef std::decimal::decimal32 dec32;
typedef std::decimal::decimal64 dec64;
typedef std::decimal::decimal128 dec128;

#include "return_x.h"

void
return_6_x (void)
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
