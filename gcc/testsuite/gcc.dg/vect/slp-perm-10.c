/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

int a[256], b[256];

void __attribute__((noinline))
foo (void)
{
  int i;
  for (i = 0; i < 32; ++i)
    {
      b[i*8+0] = a[i*8+0];
      b[i*8+1] = a[i*8+0];
      b[i*8+2] = a[i*8+3];
      b[i*8+3] = a[i*8+3];
      b[i*8+4] = a[i*8+4];
      b[i*8+5] = a[i*8+6];
      b[i*8+6] = a[i*8+4];
      b[i*8+7] = a[i*8+6];
    }
}

int main ()
{
  int i;

  check_vect ();

  for (i = 0; i < 256; ++i)
    {
      a[i] = i;
      __asm__ volatile ("");
    }

  foo ();

#pragma GCC novector
  for (i = 0; i < 32; ++i)
    if (b[i*8+0] != i*8+0
	|| b[i*8+1] != i*8+0
	|| b[i*8+2] != i*8+3
	|| b[i*8+3] != i*8+3
	|| b[i*8+4] != i*8+4
	|| b[i*8+5] != i*8+6
	|| b[i*8+6] != i*8+4
	|| b[i*8+7] != i*8+6)
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_perm } } } */
/* SLP fails for variable-length SVE because the load size is greater
   than the minimum vector size.  */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { target vect_perm xfail { { aarch64_sve || riscv_v } && vect_variable_length } } } } */
