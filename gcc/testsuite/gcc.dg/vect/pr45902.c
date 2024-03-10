/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128

short res[N];
short a[N];

int
main1 ()
{
  int i;

  for (i = 0; i < N/4; i+=4)
    {
      res[i] = a[i] >> 8;
      res[i+1] = a[i+1] >> 8;
      res[i+2] = a[i+2] >> 8;
      res[i+3] = a[i+3] >> 8;
    }
}

int
main ()
{
  int i;

  check_vect ();

  for (i = 0; i < N; i++)
    a[i] = i;

  main1 ();

#pragma GCC novector
  for (i = 0; i < N; i++)
    if (res[i] != a[i] >> 8)
      abort ();

  return 0;
}

