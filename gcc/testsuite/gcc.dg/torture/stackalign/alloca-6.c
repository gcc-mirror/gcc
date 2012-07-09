/* PR middle-end/45234 */
/* { dg-do run { target { { i?86-*-* x86_64-*-* } && ia32 } } } */
/* { dg-additional-options "-mincoming-stack-boundary=2 -mpreferred-stack-boundary=2" } */

#include "check.h"

void
__attribute__ ((noinline))
bar (__float128 f)
{
  check (&f, __alignof__(f));
}

volatile int z = 6;

int
main (void)
{
  char *p = __builtin_alloca (z);

  bar (0);

  __builtin_strncpy (p, "good", 5);
  if (__builtin_strncmp (p, "good", 5) != 0)
    {
#ifdef DEBUG
      p[z - 1] = '\0';
      printf ("Failed: %s != good\n", p);
#endif
     abort ();
    }

  return 0;
}
