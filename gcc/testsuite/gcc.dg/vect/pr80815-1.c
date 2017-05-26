/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"
int arr[2048];

__attribute__ ((noinline)) int
foo (int *a, int *b)
{
  int i;
  int *a1 = a;
  int *a0 = a1 - 512;
  for (i = 0; i < 500; i++)
    {
      *b = *a0 + *a1;
      b++;
      a0--;
      a1--;
    }
  return 0;
}

int main (void)
{
  int *a = &arr[1027];
  int *b = &arr[1024];

  int i;
  for (i = 0; i < 2048; i++)
    arr[i] = i;

  foo (a, b);

  if (arr[1026] != 2053 || arr[1027] != 2054)
    abort ();

  return 0;
}

