/* PR debug/49580 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=4 -fdump-tree-parloops-details" } */

#include <stdarg.h>
#include <stdlib.h>

#define N 1600

unsigned int ub[N];
unsigned char reg_has_output_reload[N];
unsigned int uc[N];

 __attribute__ ((noinline)) 
 void main2 (unsigned int regno, unsigned int n_reloads)
 {
  unsigned int nr=0; 

  if (regno> ub[regno])
    nr=regno;
  else
    nr=ub[nr];

  while (nr-- > 0)
    if (n_reloads == 0 || reg_has_output_reload[regno + nr] == 0)
      ub[regno + nr] = 0;
}

int main (void)
{ 
  main2 (799, 0);
  return 0;
}


/* { dg-final { scan-tree-dump-times "SUCCESS: may be parallelized" 1 "parloops" } } */
/* { dg-final { cleanup-tree-dump "parloops" } } */

