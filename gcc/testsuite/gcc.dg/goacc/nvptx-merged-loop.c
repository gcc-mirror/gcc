/* { dg-do link } */
/* { dg-require-effective-target offload_nvptx } */
/* { dg-options "-fopenacc -O2 -foffload=-fdump-rtl-mach\\ -dumpbase\\ nvptx-merged-loop.c\\ -Wa,--no-verify" } */

#define N (32*32*32+17)
void __attribute__ ((noinline)) Foo (int *ary)
{
  int ix;

#pragma acc parallel num_workers(32) vector_length(32) copyout(ary[0:N])
  {
    /* Loop partitioning should be merged.  */
#pragma acc loop worker vector
    for (unsigned ix = 0; ix < N; ix++)
      {
	ary[ix] = ix;
      }
  }
}

int main ()
{
  int ary[N];

  Foo (ary);

  return 0;
}   

/* { dg-final { scan-rtl-dump "Merging loop .* into " "mach" } } */
