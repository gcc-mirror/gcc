// PR middle-end/113436
// { dg-do "compile" }
// { dg-options "-fopenmp -fdump-tree-omplower" }

void f(int x)
{
  int a[x], *b[x];
  int (&aRef)[x] = a;
  int *(&bRef)[x] = b;

  #pragma omp target private (aRef, bRef)
  {
    aRef[0] = 0;
    bRef[0] = 0;
  }
}

// Ensure that the size of memory allocated for the VLA is from a variable
// rather than a constant, and that the default alignments are as expected.

// { dg-final { scan-tree-dump "a\\\.\[0-9\]\+ = __builtin_alloca_with_align \\\(D\\\.\[0-9\]\+, 32\\\);" "omplower" { target int32 } } }
// { dg-final { scan-tree-dump "b\\\.\[0-9\]\+ = __builtin_alloca_with_align \\\(D\\\.\[0-9\]\+, 32\\\);" "omplower" { target ilp32 } } }
// { dg-final { scan-tree-dump "b\\\.\[0-9\]\+ = __builtin_alloca_with_align \\\(D\\\.\[0-9\]\+, 64\\\);" "omplower" { target { lp64 || llp64 } } } }
// { dg-final { scan-tree-dump "D\\\.\[0-9\]\+ = __builtin_alloca_with_align \\\(D\\\.\[0-9\]\+, 32\\\);" "omplower" { target int32 } } }
// { dg-final { scan-tree-dump "D\\\.\[0-9\]\+ = __builtin_alloca_with_align \\\(D\\\.\[0-9\]\+, 32\\\);" "omplower" { target ilp32 } } }
// { dg-final { scan-tree-dump "D\\\.\[0-9\]\+ = __builtin_alloca_with_align \\\(D\\\.\[0-9\]\+, 64\\\);" "omplower" { target { lp64 || llp64 } } } }
// { dg-final { scan-tree-dump "aRef = D\\\.\[0-9\]\+;" "omplower" } }
// { dg-final { scan-tree-dump "bRef = D\\\.\[0-9\]\+;" "omplower" } }
