/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

int f (int a, void *b, float c[2]);

#pragma omp declare variant (f) match (construct={dispatch}) adjust_args (nothing: a) adjust_args (need_device_ptr: b, c)
int f0 (int a, void *b, float c[2]);
#pragma omp declare variant (f) match (construct={dispatch}) adjust_args (nothing: a) adjust_args (need_device_ptr: b) adjust_args (need_device_ptr: c)
int f1 (int a, void *b, float c[2]);

int test () {
  int a;
  void *b;
  float c[2];
  struct {int a;} s;

  s.a = f0 (a, b, c);
  #pragma omp dispatch
  s.a = f0 (a, b, c);

  f1 (a, b, c);
  #pragma omp dispatch
  s.a = f1 (a, b, c);

  return s.a;
}

/* { dg-final { scan-tree-dump-times "__builtin_omp_get_default_device \\(\\);" 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "D\.\[0-9]+ = __builtin_omp_get_mapped_ptr \\(&c, D\.\[0-9]+\\);" 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "D\.\[0-9]+ = __builtin_omp_get_mapped_ptr \\(b, D\.\[0-9]+\\);" 2 "gimple" } } */
