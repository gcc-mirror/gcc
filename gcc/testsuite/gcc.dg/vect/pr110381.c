/* { dg-do run } */
/* { dg-require-effective-target vect_float_strict } */

#include "tree-vect.h"

struct FOO {
   double a;
   double b;
   double c;
};

double __attribute__((noipa))
sum_8_foos(const struct FOO* foos)
{
  double sum = 0;

  for (int i = 0; i < 8; ++i)
    {
      struct FOO foo = foos[i];

      /* Need to use an in-order reduction here, preserving
         the load permutation.  */
      sum += foo.a;
      sum += foo.c;
      sum += foo.b;
    }

  return sum;
}

int main()
{
  struct FOO foos[8];

  check_vect ();

  __builtin_memset (foos, 0, sizeof (foos));
  foos[0].a = __DBL_MAX__;
  foos[0].b = 5;
  foos[0].c = -__DBL_MAX__;

  if (sum_8_foos (foos) != 5)
    __builtin_abort ();
  return 0;
}
