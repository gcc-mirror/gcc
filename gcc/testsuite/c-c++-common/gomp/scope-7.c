/* { dg-do compile } */
/* { dg-additional-options "-fopenmp-ompt=basic -fdump-tree-omplower" } */

/* Check that OMPT variants of libgomp calls are emitted for the scope
   construct, both with and without a task reduction clause.  */

int x;

void
f1 (void)
{
  #pragma omp scope
  ;
}

void
f2 (void)
{
#pragma omp scope reduction(task, + : x)
  ;
}

/* { dg-final { scan-tree-dump-times "GOMP_scope_start_with_end \\(0B\\)" 1 "omplower" } } */
/* { dg-final { scan-tree-dump-times "GOMP_scope_start_with_end \\(D\.\[0-9\]+\\)" 1 "omplower" } } */
/* { dg-final { scan-tree-dump-times "GOMP_scope_end" 2 "omplower" } } */
/* { dg-final { scan-tree-dump-not "GOMP_scope_start \\(" "omplower" } } */
