/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

float b[N*2] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69,72,75,78,81,84,87,90,93};
float c[N*2] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31};

__attribute__ ((noinline)) int
main1 (void)
{
  int i;
  float a[N*2];

  /* Strided access pattern.  */
  for (i = 0; i < N/2; i++)
    {
      a[i*2] = b[2*i+1] * c[2*i+1] - b[2*i] * c[2*i];
      a[i*2+1] = b[2*i+8] * c[2*i+9] + b[2*i+9] * c[2*i+8];
    }

  /* Check results.  */
#pragma GCC novector
  for (i = 0; i < N/2; i++)
    {
      if (a[i*2] != b[2*i+1] * c[2*i+1] - b[2*i] * c[2*i]
	  || a[i*2+1] != b[2*i+8] * c[2*i+9] + b[2*i+9] * c[2*i+8])
	abort();
    }

  return 0;
}

int main (void)
{
  check_vect ();
  return main1 ();
}

/* Needs interleaving support.  */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_strided2 } } } */
/* { dg-final { scan-tree-dump-times "vectorized 0 loops" 1 "vect" { xfail vect_strided2 } } } */
  
