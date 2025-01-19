/* OpenACC default (present) clause.  */

/* { dg-additional-options "-fdump-tree-gimple" } */

void f1 ()
{
  int f1_a = 2, f1_c = 3;
  float f1_b[2], f1_d[2];

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
#pragma acc serial default (present)
  /* { dg-final { scan-tree-dump-times "omp target oacc_serial default\\(present\\) map\\(force_present:f1_b \[^\\)\]+\\) firstprivate\\(f1_a\\)" 1 "gimple" } } */
  {
    f1_b[0] = f1_a;
  }

  /* { dg-final { scan-tree-dump-times "omp target oacc_data default\\(present\\)" 3 "gimple" } } */
#pragma acc data default (present)
#pragma acc kernels
  /* { dg-final { scan-tree-dump-times "omp target oacc_kernels map\\(force_present:f1_d \[^\\)\]+\\) map\\(force_tofrom:f1_c" 1 "gimple" } } */
  {
    f1_d[0] = f1_c;
  }
#pragma acc data default (none)
#pragma acc data default (present)
#pragma acc parallel
  /* { dg-final { scan-tree-dump-times "omp target oacc_parallel map\\(force_present:f1_d \[^\\)\]+\\) firstprivate\\(f1_c\\)" 1 "gimple" } } */
  {
    f1_d[0] = f1_c;
  }
#pragma acc data default (none)
#pragma acc data default (none)
#pragma acc data default (present)
#pragma acc serial
  /* { dg-final { scan-tree-dump-times "omp target oacc_serial map\\(force_present:f1_d \[^\\)\]+\\) firstprivate\\(f1_c\\)" 1 "gimple" } } */
  {
    f1_d[0] = f1_c;
  }
}
