/* { dg-do compile } */

typedef unsigned int T;

#define SWAP(A, B) do { T tmp = A; A = B; B = tmp; } while (0)

void
insertion_sort(T *v, int n)
{
  for (int i = 1; i < n; ++i)
    for (int k = i; k > 0 && v[k-1] > v[k]; --k)
      SWAP(v[k-1], v[k]);
}

/* { dg-final { scan-tree-dump "using element-wise load" "slp1" { target { { x86_64-*-* i?86-*-* } && { ! ia32 } } } } } */
