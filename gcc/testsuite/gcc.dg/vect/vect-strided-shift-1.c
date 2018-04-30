/* { dg-skip-if "Skip for mips -mpaired-single" { mips*-*-* } { "-mpaired-single" } } */
/* PR tree-optimization/65963.  */
#include "tree-vect.h"

#define N 512

int in[2*N], out[N];

__attribute__ ((noinline)) void
loop (void)
{
  for (int i = 0; i < N; i++)
    out[i] = in[i << 1] + 7;
}

int
main (int argc, char **argv)
{
  check_vect ();
  for (int i = 0; i < 2*N; i++)
    {
      in[i] = i;
      __asm__ volatile ("" : : : "memory");
    }
  loop ();
  __asm__ volatile ("" : : : "memory");
  for (int i = 0; i < N; i++)
    {
      if (out[i] != i*2 + 7)
	abort ();
    }
  return 0;
}
/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 1 "vect" { target { vect_strided2 } } } } */
