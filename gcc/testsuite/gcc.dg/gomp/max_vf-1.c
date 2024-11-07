/* Test that omp parallel simd schedule uses the correct max_vf for the
   host system, when no target directives are present.  */

/* { dg-do compile } */
/* { dg-options "-fopenmp -O2 -fdump-tree-ompexp" } */

/* Fix a max_vf size so we can scan for it.
{ dg-additional-options "-msse2 -mno-avx" { target { x86_64-*-* i?86-*-* } } } */

#define N 1024
int a[N], b[N], c[N];

void
f2 (void)
{
  int i;
  #pragma omp parallel for simd schedule (simd: static, 7)
  for (i = 0; i < N; i++)
    a[i] = b[i] + c[i];
}

/* Make sure the max_vf is inlined as a number.
   Hopefully there are no unrelated uses of these numbers ...
{ dg-final { scan-tree-dump-times {\* 16} 2 "ompexp" { target { x86_64-*-* } } } }
{ dg-final { scan-tree-dump-times {\+ 16} 1 "ompexp" { target { x86_64-*-* } } } } */

void
f3 (int *a, int *b, int *c)
{
  int i;
  #pragma omp parallel for simd schedule (simd : dynamic, 7)
  for (i = 0; i < N; i++)
    a[i] = b[i] + c[i];
}

/* Make sure the max_vf is inlined as a number.
{ dg-final { scan-tree-dump-times {__builtin_GOMP_parallel_loop_nonmonotonic_dynamic \(.*, 16, 0\);} 1 "ompexp" { target { x86_64-*-* } } } } */
