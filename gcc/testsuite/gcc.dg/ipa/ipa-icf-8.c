/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf"  } */

#include <stdio.h>

__attribute__ ((noinline))
int fce1(int a, int b)
{
  int swap;

  if(a < b)
    {
      swap = a;
      a = b;
      b = swap;
    }

  return a / b;
}

__attribute__ ((noinline))
int fce2(int x, int y)
{
  int tmp;

  if(x < y)
    {
      tmp = x;
      x = y;
      y = tmp;
    }

  return x / y;
}


int main(int argc, char **argv)
{
  printf("fce1: %d\n", fce1(argc, argc + 2));
  printf("fce2: %d\n", fce2(argc, 2 * argc));
}

/* { dg-final { scan-ipa-dump "Semantic equality hit:fce1->fce2" "icf"  } } */
/* { dg-final { scan-ipa-dump "Equal symbols: 1" "icf"  } } */
