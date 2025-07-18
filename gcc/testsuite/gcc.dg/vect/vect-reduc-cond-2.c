/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_condition } */
/* { dg-additional-options "-fdump-tree-ifcvt-details" } */

#include <stdarg.h>
#include "tree-vect.h"

/* PR tree-optimization/119920 */

#define N 32

unsigned int ub[N];
unsigned int ua[N];

/* Test vectorization of reduction of unsigned-int.  */

__attribute__ ((noinline, noipa))
void init(void)
{
  #pragma GCC novector
  for(int i = 0;i < N; i++) {
    ub[i] = i;
    ua[i] = 1;
  }
}


__attribute__ ((noinline, noipa))
void main1 (unsigned int b, unsigned int c)
{
  int i;
  unsigned int usum = 0;

  init();

  /* Summation.  */
  for (i = 0; i < N; i++) {
    unsigned t = ua[i];
    if ( ub[i] < N/2 )
      usum += b * t;
    else
      usum += c * t;
  }

  /* check results:  */
  /* __builtin_printf("%d : %d\n", usum, (N/2*b*1 + N/2*c*1)); */
  if (usum != N/2*b + N/2*c)
    abort ();
}

int main (void)
{ 
  check_vect ();
  
  main1 (0, 0);
  main1 (1, 1);
  main1 (10, 1);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail { vect_no_int_add } } } } */
/* { dg-final { scan-tree-dump-times "changed to factor operation out from COND_EXPR" 2 "ifcvt" } } */
