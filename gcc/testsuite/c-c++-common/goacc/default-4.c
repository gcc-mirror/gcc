/* OpenACC default clause inside data construct.  */

/* { dg-additional-options "-fdump-tree-gimple" } */

void f1 ()
{
  int f1_a = 2;
  float f1_b[2];

#pragma acc data copyin (f1_a) copyout (f1_b)
  /* { dg-final { scan-tree-dump-times "omp target oacc_data map\\(force_from:f1_b \[^\\)\]+\\) map\\(force_to:f1_a" 1 "gimple" } } */
  {
#pragma acc kernels
    /* { dg-final { scan-tree-dump-times "omp target oacc_kernels map\\(tofrom:f1_b \[^\\)\]+\\) map\\(tofrom:f1_a" 1 "gimple" } } */
    {
      f1_b[0] = f1_a;
    }
#pragma acc parallel
    /* { dg-final { scan-tree-dump-times "omp target oacc_parallel map\\(tofrom:f1_b \[^\\)\]+\\) map\\(tofrom:f1_a" 1 "gimple" } } */
    {
      f1_b[0] = f1_a;
    }
  }
}

void f2 ()
{
  int f2_a = 2;
  float f2_b[2];

#pragma acc data copyin (f2_a) copyout (f2_b)
  /* { dg-final { scan-tree-dump-times "omp target oacc_data map\\(force_from:f2_b \[^\\)\]+\\) map\\(force_to:f2_a" 1 "gimple" } } */
  {
#pragma acc kernels default (none)
    /* { dg-final { scan-tree-dump-times "omp target oacc_kernels default\\(none\\) map\\(tofrom:f2_b \[^\\)\]+\\) map\\(tofrom:f2_a" 1 "gimple" } } */
    {
      f2_b[0] = f2_a;
    }
#pragma acc parallel default (none)
    /* { dg-final { scan-tree-dump-times "omp target oacc_parallel default\\(none\\) map\\(tofrom:f2_b \[^\\)\]+\\) map\\(tofrom:f2_a" 1 "gimple" } } */
    {
      f2_b[0] = f2_a;
    }
  }
}
