/* OpenACC default clause inside data construct.  */

/* { dg-additional-options "-fdump-tree-gimple" } */

void f1 ()
{
  int f1_a = 2;
  float f1_b[2];

#pragma acc data copyin (f1_a) copyout (f1_b)
  /* { dg-final { scan-tree-dump-times "omp target oacc_data map\\(from:f1_b \[^\\)\]+\\) map\\(to:f1_a" 1 "gimple" } } */
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
#pragma acc serial
    /* { dg-final { scan-tree-dump-times "omp target oacc_serial map\\(tofrom:f1_b \[^\\)\]+\\) map\\(tofrom:f1_a" 1 "gimple" } } */
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
  /* { dg-final { scan-tree-dump-times "omp target oacc_data map\\(from:f2_b \[^\\)\]+\\) map\\(to:f2_a" 1 "gimple" } } */
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
#pragma acc serial default (none)
    /* { dg-final { scan-tree-dump-times "omp target oacc_serial default\\(none\\) map\\(tofrom:f2_b \[^\\)\]+\\) map\\(tofrom:f2_a" 1 "gimple" } } */
    {
      f2_b[0] = f2_a;
    }
  }
}

void f2_ ()
{
  int f2__a = 2;
  float f2__b[2];

#pragma acc data default (none) copyin (f2__a) copyout (f2__b)
  /* { dg-final { scan-tree-dump-times "omp target oacc_data map\\(from:f2__b \[^\\)\]+\\) map\\(to:f2__a \[^\\)\]+\\) default\\(none\\)" 1 "gimple" } } */
  {
#pragma acc kernels
    /* { dg-final { scan-tree-dump-times "omp target oacc_kernels map\\(tofrom:f2__b \[^\\)\]+\\) map\\(tofrom:f2__a" 1 "gimple" } } */
    {
      f2__b[0] = f2__a;
    }
#pragma acc parallel
    /* { dg-final { scan-tree-dump-times "omp target oacc_parallel map\\(tofrom:f2__b \[^\\)\]+\\) map\\(tofrom:f2__a" 1 "gimple" } } */
    {
      f2__b[0] = f2__a;
    }
#pragma acc serial
    /* { dg-final { scan-tree-dump-times "omp target oacc_serial map\\(tofrom:f2__b \[^\\)\]+\\) map\\(tofrom:f2__a" 1 "gimple" } } */
    {
      f2__b[0] = f2__a;
    }
  }
}

void f3 ()
{
  int f3_a = 2;
  float f3_b[2];

#pragma acc data copyin (f3_a) copyout (f3_b)
  /* { dg-final { scan-tree-dump-times "omp target oacc_data map\\(from:f3_b \[^\\)\]+\\) map\\(to:f3_a" 1 "gimple" } } */
  {
#pragma acc kernels default (present)
    /* { dg-final { scan-tree-dump-times "omp target oacc_kernels default\\(present\\) map\\(tofrom:f3_b \[^\\)\]+\\) map\\(tofrom:f3_a" 1 "gimple" } } */
    {
      f3_b[0] = f3_a;
    }
#pragma acc parallel default (present)
    /* { dg-final { scan-tree-dump-times "omp target oacc_parallel default\\(present\\) map\\(tofrom:f3_b \[^\\)\]+\\) map\\(tofrom:f3_a" 1 "gimple" } } */
    {
      f3_b[0] = f3_a;
    }
#pragma acc serial default (present)
    /* { dg-final { scan-tree-dump-times "omp target oacc_serial default\\(present\\) map\\(tofrom:f3_b \[^\\)\]+\\) map\\(tofrom:f3_a" 1 "gimple" } } */
    {
      f3_b[0] = f3_a;
    }
  }
}

void f3_ ()
{
  int f3__a = 2;
  float f3__b[2];

#pragma acc data default (present) copyin (f3__a) copyout (f3__b)
  /* { dg-final { scan-tree-dump-times "omp target oacc_data map\\(from:f3__b \[^\\)\]+\\) map\\(to:f3__a \[^\\)\]+\\) default\\(present\\)" 1 "gimple" } } */
  {
#pragma acc kernels
    /* { dg-final { scan-tree-dump-times "omp target oacc_kernels map\\(tofrom:f3__b \[^\\)\]+\\) map\\(tofrom:f3__a" 1 "gimple" } } */
    {
      f3__b[0] = f3__a;
    }
#pragma acc parallel
    /* { dg-final { scan-tree-dump-times "omp target oacc_parallel map\\(tofrom:f3__b \[^\\)\]+\\) map\\(tofrom:f3__a" 1 "gimple" } } */
    {
      f3__b[0] = f3__a;
    }
#pragma acc serial
    /* { dg-final { scan-tree-dump-times "omp target oacc_serial map\\(tofrom:f3__b \[^\\)\]+\\) map\\(tofrom:f3__a" 1 "gimple" } } */
    {
      f3__b[0] = f3__a;
    }
  }
}
