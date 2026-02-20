// PR middle-end/113436
// { dg-do "compile" }
// { dg-options "-fopenmp -fdump-tree-omplower" }

void f(int x)
{
  int a[x];
  int (&c)[x] = a;

  #pragma omp target private (c)
  {
    c[0] = 1;
  }
}

// Ensure that the size of memory allocated for the VLA is from a variable rather than a constant.
// { dg-final { scan-tree-dump "D\\\.\[0-9\]\+ = __builtin_alloca_with_align \\\(D\\\.\[0-9\]\+, \[0-9\]\+\\\);" "omplower" } }
