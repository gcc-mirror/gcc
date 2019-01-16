// { dg-do compile }
// { dg-additional-options "-fdump-tree-original" }
// { dg-final { scan-tree-dump-times "omp atomic release" 5 "original" } }
// { dg-final { scan-tree-dump-times "omp atomic seq_cst" 1 "original" } }
// { dg-final { scan-tree-dump-times "omp atomic relaxed" 2 "original" } }
// { dg-final { scan-tree-dump-times "omp atomic capture acq_rel" 3 "original" } }
// { dg-final { scan-tree-dump-times "omp atomic capture acquire" 1 "original" } }
// { dg-final { scan-tree-dump-times "omp atomic read acquire" 1 "original" } }

int i, v;
float f;

template <int N, int M, typename T>
void
foo (T *p)
{
  #pragma omp atomic release, hint (N), update
  i = i + 1;
  #pragma omp atomic hint(0)seq_cst
  i = i + 1;
  #pragma omp atomic relaxed,update,hint (N)
  i = i + 1;
  #pragma omp atomic release
  i = i + 1;
  #pragma omp atomic relaxed
  i = i + 1;
  #pragma omp atomic acq_rel capture
  v = i = i + 1;
  #pragma omp atomic capture,acq_rel , hint (M)
  v = i = i + 1;
  #pragma omp atomic hint(N),acquire capture
  v = i = i + 1;
  #pragma omp atomic read acquire
  v = i;
  #pragma omp atomic release,write
  i = v;
  #pragma omp atomic hint(1),update,release
  f = f + 2.0;
  #pragma omp requires atomic_default_mem_order (acq_rel)
  #pragma omp atomic hint (M - 1) update
  *p += 1;
  #pragma omp atomic capture, hint (M)
  v = *p = *p + 1;
}

void
bar ()
{
  foo <0, 1, int> (&i);
}
