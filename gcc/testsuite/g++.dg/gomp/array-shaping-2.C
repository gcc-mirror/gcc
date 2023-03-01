// { dg-do compile }
// { dg-additional-options "-fdump-tree-original" }

template<typename T>
struct St
{
  T ***ppptr;
  T ***&rppptr;

  St(T ***p, T ***&rp) : ppptr(p), rppptr(rp) { }
};

template<typename A, typename B>
void foo()
{
  A *ptr;
  A **pptr = &ptr;
  A ***ppptr = &pptr;
  A ***&rppptr = ppptr;

#pragma omp target update to(([10]) (**ppptr)[3:4:2])
// { dg-final { scan-tree-dump {map\(to_grid:VIEW_CONVERT_EXPR<int\[10\]>\(\*\*\*ppptr\) \[len: [0-9]+\]\) map\(grid_dim:3 \[len: 4\]\) map\(grid_stride:2\)} "original" } }

#pragma omp target update to(([10]) (**rppptr)[3:4:2])
// { dg-final { scan-tree-dump {map\(to_grid:VIEW_CONVERT_EXPR<int\[10\]>\(\*\*\*\*rppptr\) \[len: [0-9]+\]\) map\(grid_dim:3 \[len: 4\]\) map\(grid_stride:2\)} "original" } }

#pragma omp target update to((**ppptr)[3:4:2])
// { dg-final { scan-tree-dump {map\(to_grid:\*\*ppptr \[len: [0-9]+\]\) map\(grid_dim:3 \[len: 4\]\) map\(grid_stride:2\)} "original" } }

#pragma omp target update to((**rppptr)[3:4:2])
// { dg-final { scan-tree-dump {map\(to_grid:\*\*\*rppptr \[len: [0-9]+\]\) map\(grid_dim:3 \[len: 4\]\) map\(grid_stride:2\)} "original" } }

  B *ptr2;
  B **pptr2 = &ptr2;
  B ***ppptr2 = &pptr2;
  St<B> *s = new St<B>(ppptr2, ppptr2);
  St<B> **ps = &s;
  St<B> **&rps = ps;

#pragma omp target update from(([10]) (**(*ps)->ppptr)[3:4:2])
// { dg-final { scan-tree-dump {map\(from_grid:VIEW_CONVERT_EXPR<long int\[10\]>\(\*\*\*\(\*ps\)->ppptr\) \[len: [0-9]+\]\) map\(grid_dim:3 \[len: 4\]\) map\(grid_stride:2\)} "original" } }

#pragma omp target update from(([10]) (**(*rps)->rppptr)[3:4:2])
// { dg-final { scan-tree-dump {map\(from_grid:VIEW_CONVERT_EXPR<long int\[10\]>\(\*\*\*\*\(\*\*rps\)->rppptr\) \[len: [0-9]+\]\) map\(grid_dim:3 \[len: 4\]\) map\(grid_stride:2\)} "original" } }

#pragma omp target update from((**(*ps)->ppptr)[3:4:2])
// { dg-final { scan-tree-dump {map\(from_grid:\*\*\(\*ps\)->ppptr \[len: [0-9]+\]\) map\(grid_dim:3 \[len: 4\]\) map\(grid_stride:2\)} "original" } }

#pragma omp target update from((**(*rps)->rppptr)[3:4:2])
// { dg-final { scan-tree-dump {map\(from_grid:\*\*\*\(\*\*rps\)->rppptr \[len: [0-9]+\]\) map\(grid_dim:3 \[len: 4\]\) map\(grid_stride:2\)} "original" } }

  B arr[10][10];
  B (*parr)[10][10] = &arr;
  B (**pparr2)[10][10] = &parr;
  B (**&rpparr2)[10][10] = pparr2;

#pragma omp target update from(**pparr2)
// { dg-final { scan-tree-dump {from\(\*NON_LVALUE_EXPR <\*pparr2> \[len: [0-9]+\]\)} "original" } }

#pragma omp target update to((**pparr2)[1:5:2][3:4:2])
// { dg-final { scan-tree-dump {map\(to_grid:\*\*pparr2 \[len: [0-9]+\]\) map\(grid_dim:1 \[len: 5\]\) map\(grid_stride:2\) map\(grid_dim:3 \[len: 4\]\) map\(grid_stride:2\)} "original" } }

#pragma omp target update from((**rpparr2)[1:5:2][3:4:2])
// { dg-final { scan-tree-dump {map\(from_grid:\*\*\*rpparr2 \[len: [0-9]+\]\) map\(grid_dim:1 \[len: 5\]\) map\(grid_stride:2\) map\(grid_dim:3 \[len: 4\]\) map\(grid_stride:2\)} "original" } }

  delete s;
}

struct S
{
  short ***ppptr;
  short ***&rppptr;

  S(short ***p, short ***&rp) : ppptr(p), rppptr(rp) { }
};

int main()
{
  char *ptr;
  char **pptr = &ptr;
  char ***ppptr = &pptr;
  char ***&rppptr = ppptr;

#pragma omp target update to(([10]) (**ppptr)[1:5:2])
// { dg-final { scan-tree-dump {map\(to_grid:VIEW_CONVERT_EXPR<char\[10\]>\(\*\*\*ppptr\) \[len: 1\]\) map\(grid_dim:1 \[len: 5\]\) map\(grid_stride:2\)} "original" } }

#pragma omp target update to(([10]) (**rppptr)[1:5:2])
// { dg-final { scan-tree-dump {map\(to_grid:VIEW_CONVERT_EXPR<char\[10\]>\(\*\*\*\*rppptr\) \[len: 1\]\) map\(grid_dim:1 \[len: 5\]\) map\(grid_stride:2\)} "original" } }

#pragma omp target update to((**ppptr)[1:5:2])
// { dg-final { scan-tree-dump {map\(to_grid:\*\*ppptr \[len: 1\]\) map\(grid_dim:1 \[len: 5\]\) map\(grid_stride:2\)} "original" } }

#pragma omp target update to((**rppptr)[1:5:2])
// { dg-final { scan-tree-dump {map\(to_grid:\*\*\*rppptr \[len: 1\]\) map\(grid_dim:1 \[len: 5\]\) map\(grid_stride:2\)} "original" } }

  short *ptr2;
  short **pptr2 = &ptr2;
  short ***ppptr2 = &pptr2;
  S *s = new S(ppptr2, ppptr2);
  S **ps = &s;
  S **&rps = ps;

#pragma omp target update from(([10]) (**(*ps)->ppptr)[1:5:2])
// { dg-final { scan-tree-dump {map\(from_grid:VIEW_CONVERT_EXPR<short int\[10\]>\(\*\*\*\(\*ps\)->ppptr\) \[len: [0-9]+\]\) map\(grid_dim:1 \[len: 5\]\) map\(grid_stride:2\)} "original" } }

#pragma omp target update from(([10]) (**(*rps)->rppptr)[1:5:2])
// { dg-final { scan-tree-dump {map\(from_grid:VIEW_CONVERT_EXPR<short int\[10\]>\(\*\*\*\*\(\*\*rps\)->rppptr\) \[len: [0-9]+\]\) map\(grid_dim:1 \[len: 5\]\) map\(grid_stride:2\)} "original" } }

#pragma omp target update from((**(*ps)->ppptr)[1:5:2])
// { dg-final { scan-tree-dump {map\(from_grid:\*\*\(\*ps\)->ppptr \[len: [0-9]+\]\) map\(grid_dim:1 \[len: 5\]\) map\(grid_stride:2\)} "original" } }

#pragma omp target update from((**(*rps)->rppptr)[1:5:2])
// { dg-final { scan-tree-dump {map\(from_grid:\*\*\*\(\*\*rps\)->rppptr \[len: [0-9]+\]\) map\(grid_dim:1 \[len: 5\]\) map\(grid_stride:2\)} "original" } }

  delete s;

  short arr[10][10];
  short (*parr)[10][10] = &arr;
  short (**pparr)[10][10] = &parr;
  short (**&rpparr)[10][10] = pparr;

#pragma omp target update from(**pparr)
// { dg-final { scan-tree-dump {from\(\*NON_LVALUE_EXPR <\*pparr> \[len: [0-9]+\]\)} "original" } }

#pragma omp target update to((**pparr)[1:5:2][1:5:2])
// { dg-final { scan-tree-dump {map\(to_grid:\*\*pparr \[len: [0-9]+\]\) map\(grid_dim:1 \[len: 5\]\) map\(grid_stride:2\) map\(grid_dim:1 \[len: 5\]\) map\(grid_stride:2\)} "original" } }

#pragma omp target update from((**rpparr)[1:5:2][1:5:2])
// { dg-final { scan-tree-dump {map\(from_grid:\*\*\*rpparr \[len: [0-9]+\]\) map\(grid_dim:1 \[len: 5\]\) map\(grid_stride:2\) map\(grid_dim:1 \[len: 5\]\) map\(grid_stride:2\)} "original" } }

  foo<int, long> ();

  return 0;
}
