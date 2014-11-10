/* { dg-do run } */
/* { dg-skip-if "Stack alignment is too small" { hppa*-*-hpux* } "*" "" } */
/* { dg-skip-if "Stack alignment causes use of alloca" { nvptx-*-* } "*" "" } */

#include "check.h"

#ifndef ALIGNMENT
#define ALIGNMENT	64
#endif

typedef int aligned __attribute__((aligned(ALIGNMENT)));

int n;

void
g (void)
{
  __label__ lab;
  void h (void)
    {
      aligned t;
      if (check_int (&t,  __alignof__(t)) != t)
	abort ();
      if (n+t == 0) goto lab;
    }
  h();
lab:
  return;
}

int main()
{
  g();
  return 0;
}
