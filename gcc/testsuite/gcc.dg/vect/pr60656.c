/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_long } */

#include "tree-vect.h"

__attribute__ ((noinline)) long
foo ()
{
  int v[] = {5000, 5001, 5002, 5003};
  long s = 0;
  int i;

  for(i = 0; i < 4; ++i)
    {
      long P = v[i];
      s += P * P * P;
    }
  return s;
}

long
bar ()
{
  int v[] = {5000, 5001, 5002, 5003};
  long s = 0;
  int i;

  for(i = 0; i < 4; ++i)
    {
      long P = v[i];
      s += P * P * P;
      __asm__ volatile ("");
    }
  return s;
}

int main()
{
  check_vect ();

  if (foo () != bar ())
    abort ();
  return 0;
}

/* P * P * P requires a widening multiplication first as well as a longxlong->long after that.  */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_widen_mult_si_to_di_pattern && vect_long_mult } } } } */
