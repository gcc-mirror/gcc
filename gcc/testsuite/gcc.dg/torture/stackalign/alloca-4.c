/* PR middle-end/37009 */
/* { dg-do run { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */
/* { dg-options "-m32 -mincoming-stack-boundary=2 -mpreferred-stack-boundary=2" } */

#include "check.h"

void
bar (char *p, int size)
{
  __builtin_strncpy (p, "good", size);
}

void
__attribute__ ((noinline))
foo (double x, double y ,double z ,double a, int size)
{
  char *p = __builtin_alloca (size + 1);
  double i;

  bar (p, size);
  if (__builtin_strncmp (p, "good", size) != 0)
    {
#ifdef DEBUG
      p[size] = '\0';
      printf ("Failed: %s != good\n", p);
#endif
     abort ();
    }

  check (&i, __alignof__(i));
}

int
main (void)
{
  double x =  1.0 ;
 
  foo (x, x, x, x, 5);

  return 0;
}
