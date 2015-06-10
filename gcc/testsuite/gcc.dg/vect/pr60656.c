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

/* { dg-final { cleanup-tree-dump "vect" } } */
