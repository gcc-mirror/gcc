/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

int f (int a, void *b, float c[2]);

#pragma omp declare variant (f) match (construct={dispatch}) adjust_args (nothing: a) adjust_args (need_device_ptr: b, c)
int f0 (int a, void *b, float c[2]);
#pragma omp declare variant (f) adjust_args (need_device_ptr: b, c) match (construct={dispatch}) adjust_args (nothing: a) 
int f1 (int a, void *b, float c[2]);

void test () {
  int a;
  void *b;
  float c[2];

  #pragma omp dispatch
  f0 (a, b, c);

  #pragma omp dispatch device (-4852)
  f0 (a, b, c);

  #pragma omp dispatch device (a + a)
  f0 (a, b, c);
}

/* { dg-final { scan-tree-dump-times "__builtin_omp_get_default_device \\(\\);" 3 "gimple" } } */
/* { dg-final { scan-tree-dump-times "D\.\[0-9]+ = __builtin_omp_get_mapped_ptr \\(&c, D\.\[0-9]+\\);" 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "D\.\[0-9]+ = __builtin_omp_get_mapped_ptr \\(b, D\.\[0-9]+\\);" 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "D\.\[0-9]+ = __builtin_omp_get_mapped_ptr \\(&c, -4852\\);" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "D\.\[0-9]+ = __builtin_omp_get_mapped_ptr \\(b, -4852\\);" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-not "#pragma omp dispatch device" "gimple" } } */
