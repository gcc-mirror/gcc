/* PR c++/119659 */
/* { dg-additional-options "-fdump-tree-gimple" }  */

/* Test correct argument gets adjusted.  */

struct S {
  void v0(int *, int *) {}

  #pragma omp declare variant(v0) match(construct={dispatch}) adjust_args(need_device_ptr: a)
  void b0(int *a, int *b) {}

  void v1(int *, int *) {}

  #pragma omp declare variant(v1) match(construct={dispatch}) adjust_args(need_device_ptr: 1)
  void b1(int *a, int *b) {}

  void v2(int *, int *) {}

  #pragma omp declare variant(v2) match(construct={dispatch}) adjust_args(need_device_ptr: 1:1)
  void b2(int *a, int *b) {}
};


void f(int *p0, int *p1, int *p2, int *p3, int *p4, int *p5)
{
  S s;
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_default_device \\(\\);" 3 "gimple" } }  */
  #pragma omp dispatch
  s.b0(p0, p1);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p0, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "S::v0 \\(&s, D\.\[0-9\]+, p1\\);" "gimple" } }  */

  #pragma omp dispatch
  s.b1(p2, p3);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p2, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "S::v1 \\(&s, D\.\[0-9\]+, p3\\);" "gimple" } }  */

  #pragma omp dispatch
  s.b2(p4, p5);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p4, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "S::v2 \\(&s, D\.\[0-9\]+, p5\\);" "gimple" } }  */
}
