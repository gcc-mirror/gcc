extern int i;

void
f_omp_target (void)
{
#pragma omp target
  {
#pragma omp target /* { dg-warning ".target. construct inside of .target. region" } */
    ;
#pragma omp target data map(i) /* { dg-warning ".target data. construct inside of .target. region" } */
    ;
#pragma omp target update to(i) /* { dg-warning ".target update. construct inside of .target. region" } */

#pragma omp parallel
    {
#pragma omp target /* { dg-warning ".target. construct inside of .target. region" } */
      ;
#pragma omp target data map(i) /* { dg-warning ".target data. construct inside of .target. region" } */
      ;
#pragma omp target update to(i) /* { dg-warning ".target update. construct inside of .target. region" } */
    }
  }
}
