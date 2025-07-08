/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

/* PR tree-optimization/112324 */
/* PR tree-optimization/110015 */

#define N 32

int ub[N];

/* Test vectorization of reduction of int max with some extra code involed.  */

__attribute__ ((noinline, noipa))
void init(void)
{
  #pragma GCC novector
  for(int i = 0;i < N; i++)
    ub[i] = (i&4) && (i&1) ? -i : i;
}

#define MAX(a, b) ((a) > (b) ? (a) : (b)) 

__attribute__ ((noinline, noipa))
void main1 (void)
{
  int i;
  int max = 0;

  init();

  /* Summation.  */
  for (i = 0; i < N; i++) {
    int tmp = ub[i];
    if (tmp < 0)
      max = MAX (-tmp, max);
    else
      max = MAX (tmp, max);
  }

  /* check results:  */
  /* __builtin_printf("%d : %d\n", max, N); */
  if (max != N - 1)
    abort ();
}

int main (void)
{ 
  check_vect ();
  
  main1 ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail { vect_no_int_min_max } } } } */
