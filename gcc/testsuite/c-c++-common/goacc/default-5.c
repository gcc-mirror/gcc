/* OpenACC default (present) clause.  */

/* { dg-additional-options "-fdump-tree-gimple" } */

void f1 ()
{
  int f1_a = 2;
  float f1_b[2];

#pragma acc kernels default (present)
  /* { dg-final { scan-tree-dump-times "omp target oacc_kernels default\\(present\\) map\\(force_present:f1_b \[^\\)\]+\\) map\\(force_tofrom:f1_a" 1 "gimple" } } */
  {
    f1_b[0] = f1_a;
  }
#pragma acc parallel default (present)
  /* { dg-final { scan-tree-dump-times "omp target oacc_parallel default\\(present\\) map\\(force_present:f1_b \[^\\)\]+\\) firstprivate\\(f1_a\\)" 1 "gimple" } } */
  {
    f1_b[0] = f1_a;
  }
}
