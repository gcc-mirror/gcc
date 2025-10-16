/* Test that GOMP_task is special cased when cpyfn is NULL.  */

/* { dg-do run } */
/* { dg-options "-O3 -fopenmp -flto -std=gnu99 -fdump-ipa-cp-details" } */
/* { dg-require-effective-target fopenmp } */
/* { dg-require-effective-target lto } */

void test(int c) {
  for (int i = 0; i < c; i++)
    if (!__builtin_constant_p(c))
      __builtin_abort();
}
int main() {
#pragma omp task
  test(7);
  return 0;
}

/* { dg-final { scan-wpa-ipa-dump "Creating a specialized node of main._omp_fn" "cp" } } */
