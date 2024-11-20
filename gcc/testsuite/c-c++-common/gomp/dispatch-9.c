/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-original -fdump-tree-gimple" } */

/* Check that the right call to f is wrapped in a GOMP_DISPATCH internal function
   before translation and that it is stripped during gimplification. */

int f(int);
void g(int *x)
{
  #pragma omp dispatch
  x[f(1)] = f(f(2));
  //        ^ only this call to f is a dispatch call
}

/* { dg-final { scan-tree-dump "\.GOMP_DISPATCH \\(f \\(f \\(2\\)\\)\\)" "original" } } */
/* { dg-final { scan-tree-dump-times "\.GOMP_DISPATCH" 1 "original" } } */
/* { dg-final { scan-tree-dump-not "\.GOMP_DISPATCH" "gimple" } } */
