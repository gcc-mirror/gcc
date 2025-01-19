/* { dg-do compile } */

/* Check that the OpenMP 5.1 syntax with commas after the directive name and
   between clauses is supported. */

int f (int a, void *b, float c[2]);

#pragma omp declare variant (f), match (construct={dispatch}), adjust_args (nothing: a), adjust_args (need_device_ptr: b, c)
int f0 (int a, void *b, float c[2]);

int test () {
  int a;
  void *b;
  float c[2];
  struct {int a;} s;

  #pragma omp dispatch, novariants(0), nocontext(1)
  s.a = f0 (a, b, c);

  return s.a;
}
