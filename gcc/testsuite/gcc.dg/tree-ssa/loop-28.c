/* { dg-do compile { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-O2 -fno-tree-vectorize -fprefetch-loop-arrays -march=amdfam10 -fdump-tree-optimized -fdump-tree-aprefetch --param max-unrolled-insns=1000" } */

char x[100000];

void foo(int n)
{
  int i;

  for (i = 0; i < n; i++)
    x[i] = (char) i;
}

/* There should be 64 MEMs in the unrolled loop and one more in the copy of the loop
   for the rest of the iterations.  */

/* { dg-final { scan-tree-dump-times "MEM" 65 "optimized" } } */

/* There should be no i_a = i_b assignments.  */
/* { dg-final { scan-tree-dump-times "i_.*= i_\[0-9\]*;" 0 "aprefetch" } } */

