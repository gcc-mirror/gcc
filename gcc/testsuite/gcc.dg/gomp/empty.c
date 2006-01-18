/* { dg-do compile } */
/* { dg-options "-O -fopenmp -fdump-tree-omplower" } */

main()
{
#pragma omp parallel
    {;}
}

/* There should not be a GOMP_parallel_start call.  */
/* { dg-final { scan-tree-dump-times "GOMP_parallel_start" 0 "omplower"} } */
/* { dg-final { cleanup-tree-dump "omplower" } } */
