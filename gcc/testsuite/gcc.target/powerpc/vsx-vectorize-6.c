/* { dg-do compile } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-mdejagnu-cpu=power7 -O2 -ftree-vectorize -fno-vect-cost-model -fdump-tree-vect-details" } */
/* { dg-require-effective-target powerpc_vsx } */

/* Taken from vect/vect-95.c.  */

#define N 256

void bar (double *pd, double *pa, double *pb, double *pc);

__attribute__ ((noinline)) int
main1 (int n, double * __restrict__ pd, double * __restrict__ pa, double * __restrict__ pb, double * __restrict__ pc)
{
  int i;

  for (i = 0; i < n; i++)
    {
      pa[i] = pb[i] * pc[i];
      pd[i] = 5.0;
    }

  bar (pd,pa,pb,pc);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 0 "vect" { xfail { {! vect_hw_misalign } || powerpc*-*-* } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning" 0 "vect" { xfail { powerpc*-*-aix* } } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 4 "vect" { xfail { {! vect_hw_misalign } || powerpc*-*-* } } } } */
