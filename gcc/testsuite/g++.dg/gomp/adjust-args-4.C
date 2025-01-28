/* { dg-additional-options "-fdump-tree-gimple" }  */

/* PR fortran/118321.  */

/* Check that adjust_args applies to the right argument,
   if C++ inserts a 'this' pointer.  */

struct t1 {
  void f1(int *x, int *y, int *z);
  #pragma omp declare variant(f1) match(construct={dispatch}) \
                                  adjust_args(need_device_ptr : y)
  void g1(int *x, int *y, int *z);
};

struct t2 {
  void f2(int *x, int *y, int *z);
  #pragma omp declare variant(f2) match(construct={dispatch}) \
                                  adjust_args(need_device_ptr : x, y, z)
  void g2(int *x, int *y, int *z);
};

struct t3 {
  void f3(int *x, int *y, int *z);
  #pragma omp declare variant(f3) match(construct={dispatch}) \
                                  adjust_args(nothing : x, y, z)
  void g3(int *x, int *y, int *z);
};


void test(int *a1, int *b1, int *c1,
          int *a2, int *b2, int *c2,
          int *a3, int *b3, int *c3)
{
  struct t1 s1;
  struct t2 s2;
  struct t3 s3;

  #pragma omp dispatch
   s1.g1 (a1, b1, c1);
  #pragma omp dispatch
   s2.g2 (a2, b2, c2);
  #pragma omp dispatch
   s3.g3 (a3, b3, c3);
}


/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_default_device \\(\\);" 2 "gimple" } }  */

/* { dg-final { scan-tree-dump-times "__builtin_omp_get_mapped_ptr" 4 "gimple" } }  */

/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(b1, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "t1::f1 \\(&s1, a1, D\.\[0-9\]+, c1\\);" "gimple" } }  */


/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(c2, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(b2, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(a2, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "t2::f2 \\(&s2, D\.\[0-9\]+, D\.\[0-9\]+, D\.\[0-9\]+\\);" "gimple" } }  */

/* { dg-final { scan-tree-dump "t3::f3 \\(&s3, a3, b3, c3\\);" "gimple" } }  */
