/* { dg-additional-options "-fdump-tree-original -fdump-tree-gimple" } */

void f()
{
  struct s {
    int i;
  };
  int scalar1 = 5;
  int array1[5] = {1,2,3,4,5};
  int *ptr1 = &scalar1;
  struct s mystruct1 = {.i = 5};

  /* firstprivate + unspecified modifer. */
  #pragma omp target defaultmap(firstprivate)
   {
     scalar1 = 1;
     array1[0] = 2;
     if (ptr1 == 0L)
       mystruct1.i = 3;
   }

  /* equivalent: firstprivate + ALL modifer. */
  #pragma omp target defaultmap(firstprivate : all)
   {
     scalar1 = 1;
     array1[0] = 2;
     if (ptr1 == 0L)
       mystruct1.i = 3;
   }

  /* tofrom + ALL modifer. */
  #pragma omp target defaultmap(tofrom : all)
   {
     scalar1 = 1;
     array1[0] = 2;
     if (ptr1 == 0L)
       mystruct1.i = 3;
   }
}

/* { dg-final { scan-tree-dump-times "#pragma omp target defaultmap\\(firstprivate\\)" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp target defaultmap\\(firstprivate:all\\)" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp target defaultmap\\(tofrom:all\\)" 1 "original" } } */

/* { dg-final { scan-tree-dump-times "#pragma omp target.* defaultmap\\(firstprivate\\) firstprivate\\(mystruct1\\) firstprivate\\(ptr1\\) firstprivate\\(array1\\) firstprivate\\(scalar1\\)" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp target.* defaultmap\\(firstprivate:all\\) firstprivate\\(mystruct1\\) firstprivate\\(ptr1\\) firstprivate\\(array1\\) firstprivate\\(scalar1\\)" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp target.* defaultmap\\(tofrom:all\\) map\\(tofrom:mystruct1 \\\[len: .\\\] \\\[runtime_implicit\\\]\\) map\\(tofrom:ptr1 \\\[len: .\\\] \\\[runtime_implicit\\\]\\) map\\(tofrom:array1 \\\[len: ..\\\] \\\[runtime_implicit\\\]\\) map\\(tofrom:scalar1 \\\[len: .\\\] \\\[runtime_implicit\\\]\\)" 1 "gimple" } } */
