/* { dg-additional-options "-mavx2" { target avx2_runtime } } */

void __attribute__((noipa))
foo (int * __restrict wsum, int * __restrict cff, int * __restrict weight)
{
  for (int i = 0; i < 16; ++i)
    {
      *wsum += weight[2*i+0];
      *cff += weight[2*i+1];
    }
}

int main()
{
  int weight[32];
  for (int i = 0; i < 32; ++i)
    weight[i] = i;
  int wsum = 0, cff = 0;
  foo (&wsum, &cff, weight);
  if (wsum != 240 || cff != 256)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 0 "vect" } } */
