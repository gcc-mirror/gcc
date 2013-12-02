// PR middle-end/59150
// { dg-do compile }
// { dg-options "-O -fopenmp-simd -fno-tree-ccp -fno-tree-copy-prop -fno-tree-dce" }

#pragma omp declare reduction (foo: int: omp_out += omp_in) initializer (omp_priv = 0)

int
foo ()
{
  int i, v, &u = v;
  #pragma omp simd reduction (foo:u)
    for (i = 0; i < 1024; i++)
      u = i;
  return u;
}

int
bar ()
{
  int i, v, &u = v;
  #pragma omp simd reduction (foo:u) safelen(1)
    for (i = 0; i < 1024; i++)
      u = i;
  return u;
}
