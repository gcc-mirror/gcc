/* { dg-do run } */
/* { dg-additional-sources "pr66655_1.cc" } */

#include "pr66655.h"

extern "C" void abort (void);

#define COOKIE 0xabcd0123

int
g (void)
{
  return COOKIE;
}

extern int f (void);

int
main (void)
{
  S::set(0);
  if (f () != COOKIE)
    abort ();
  return 0;
}
