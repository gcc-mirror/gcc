/* { dg-additional-options "-fdump-tree-original" } */

/* In OpenMP 5.2 permits tofrom for enter/exit data
   in the FE, it is already converted to 'to' and 'from', respectively. */
int x, y, z;

void
copyin ()
{
  #pragma omp target enter data map(x) map(tofrom: y) map(always, tofrom: z)
}

void
copyout ()
{
  #pragma omp target exit data map(x) map(tofrom: y) map(always, tofrom: z)
}

/* { dg-final { scan-tree-dump-times "#pragma omp target enter data map\\(always,to:z\\) map\\(to:y\\) map\\(to:x\\)" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp target exit data map\\(always,from:z\\) map\\(from:y\\) map\\(from:x\\)" 1 "original" } } */
