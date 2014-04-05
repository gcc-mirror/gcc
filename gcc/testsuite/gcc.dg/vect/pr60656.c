/* { dg-require-effective-target vect_int } */

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
      s += P*P*P;
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
      s += P*P*P;
      __asm__ volatile ("");
    }
  return s;
}

int main()
{
  if (foo () != bar ())
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

