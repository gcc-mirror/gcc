/* { dg-additional-options "-fdump-tree-original" }  */
int var[20];

int *
iterator(int i)
{
  return &var[i];
}

void
foo (void)
{
  int iterator[10], i;
  #pragma omp task affinity(iterator(i=4:2) : iterator[i] )
   ;
  #pragma omp task affinity(iterator)
   ;
  #pragma omp task affinity(iterator[4:3])
   ;
}

void
bar (void)
{
  int j = 3;
   ;
  #pragma omp task affinity(iterator(i=4:2) : iterator(i)[2] )
   ;
  #pragma omp task affinity(iterator(j)[4])
   ;
}

/* { dg-final { scan-tree-dump-times "#pragma omp task affinity\\(iterator\\(int i=4:2:1\\):iterator\\\[SAVE_EXPR <i>\\\]\\)" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp task affinity\\(iterator\\)" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp task affinity\\(iterator\\\[4\\\]\\)" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp task affinity\\(iterator\\(int i=4:2:1\\):\\*\\(iterator \\(i\\) \\+ 8\\)\\)" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp task affinity\\(\\*\\(iterator \\(j\\) \\+ 16\\)\\)" 1 "original" } } */
