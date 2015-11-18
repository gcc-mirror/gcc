/* { dg-do link } */
/* { dg-require-effective-target offload_nvptx } */
/* { dg-options "-fopenacc -O2 -foffload=-fdump-rtl-mach\\ -dumpbase\\ nvptx-sese-1.c\\ -Wa,--no-verify" } */

#pragma acc routine  seq
int __attribute__((noinline)) foo (int x)
{
  return x & 2;
}

int main ()
{
  int r = 0;
  
#pragma acc parallel copy(r) vector_length(32)
  {
#pragma acc loop vector reduction (+:r)
    for (int i = 00; i < 40; i++)
      r += i;

    /* This piece is a multi-block SESE region */
    if (foo (r))
      r *= 2;

    if (r & 1) /* to here. */
#pragma acc loop vector reduction (+:r)
      for (int i = 00; i < 40; i++)
	r += i;
  }

  return 0;
}

/* Match {N->N(.N)+} */
/* { dg-final { scan-rtl-dump "SESE regions:.* \[0-9\]+{\[0-9\]+->\[0-9\]+(\\.\[0-9\]+)+}" "mach" } } */
