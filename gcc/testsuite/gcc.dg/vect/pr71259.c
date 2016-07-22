/* PR tree-optimization/71259 */
/* { dg-additional-options "-O3" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#include "tree-vect.h"

long a, b[1][44][2];
long long c[44][17][2];

int
main ()
{
  int i, j, k;
  check_vect ();
  asm volatile ("" : : : "memory");
  for (i = 0; i < 44; i++)
    for (j = 0; j < 17; j++)
      for (k = 0; k < 2; k++)
	c[i][j][k] = (30995740 >= *(k + *(j + *b)) != (a != 8)) - 5105075050047261684;
  asm volatile ("" : : : "memory");
  for (i = 0; i < 44; i++) 
    for (j = 0; j < 17; j++)
      for (k = 0; k < 2; k++)
	if (c[i][j][k] != -5105075050047261684)
	  __builtin_abort ();
  return 0;
}
