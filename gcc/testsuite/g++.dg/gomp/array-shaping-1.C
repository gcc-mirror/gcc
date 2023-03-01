// { dg-do compile }
// { dg-additional-options "-fdump-tree-original" }

template<typename T, typename E, int A, int B, int C, int D>
void foo ()
{
  T *ptr;
  E a = A, b = B, c = C, d = D;

  /* Dependent types for indices.  */
#pragma omp target update from(([a][b+1][c][d]) ptr[1:a-2][1:b][1:c-2][1:d-2])
// { dg-final { scan-tree-dump {map\(from_grid:VIEW_CONVERT_EXPR.*\(\*ptr\) \[len: 1\]\) map\(grid_dim:1 \[len: [^\]]+\]\) map\(grid_dim:1 \[len: [^\]]+\]\) map\(grid_dim:1 \[len: [^\]]+\]\) map\(grid_dim:1 \[len: [^]]+\]\)} "original" } }
}

int main()
{
  char *ptr;

  foo<char, short, 3, 4, 5, 6> ();

  return 0;
}
