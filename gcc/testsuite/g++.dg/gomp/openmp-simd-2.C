/* { dg-do compile } */
/* { dg-options "-fopenmp-simd -fdump-tree-original" } */

extern void abort ();
int a[1024] __attribute__((aligned (32))) = { 1 };
struct S { int s; };
#pragma omp declare reduction (+:struct S:omp_out.s += omp_in.s)
#pragma omp declare reduction (foo:struct S:omp_out.s += omp_in.s)
#pragma omp declare reduction (foo:int:omp_out += omp_in)

__attribute__((noinline, noclone)) int
foo (void)
{
  int i, u = 0;
  struct S s, t;
  s.s = 0; t.s = 0;
  #pragma omp simd aligned(a : 32) reduction(+:s) reduction(foo:t, u)
  for (i = 0; i < 1024; i++)
    {
      int x = a[i];
      s.s += x;
      t.s += x;
      u += x;
    }
  if (t.s != s.s || u != s.s)
    abort ();
  return s.s;
}


void bar(int n, float *a, float *b)
{
  int i; 
#pragma omp parallel for simd num_threads(4) safelen(64)
  for (i = 0; i < n ; i++)
    a[i] = b[i];
}

/* { dg-final { scan-tree-dump-times "Function void omp declare reduction operator\\+" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "Function void omp declare reduction foo" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "pragma omp simd reduction\\(u\\) reduction\\(t\\) reduction\\(\\+:s\\) aligned\\(a:32\\)" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "pragma omp simd safelen\\(64\\)" 1 "original" } } */
/* { dg-final { scan-tree-dump-not "omp parallel" "original" } } */
/* { dg-final { scan-tree-dump-not "omp for" "original" } } */
