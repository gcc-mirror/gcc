/* { dg-do run } */
/* { dg-additional-sources "pr66655_1.C" } */

#include "pr66655.h"

extern "C" void abort (void);

#define COOKIE 0xabcd0123

int32_t
g (void)
{
  return COOKIE;
}

extern int32_t f (void);

int
main (void)
{
  S::set(0);
  if (f () != COOKIE)
    abort ();
  return 0;
}
