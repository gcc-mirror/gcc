/* { dg-additional-options "-fdump-tree-gimple" }  */

/* Valid constexpr variable/NTTP in numeric range.
   TODO: add tests including multiple instantiations of the same function template.  */

const int G = 2;

template<int>
struct S1 {};

template<int, int>
struct S2 {};


void v0(int *, int *) {}

#pragma omp declare variant(v0) match(construct={dispatch}) \
				adjust_args(need_device_ptr: G:omp_num_args)
void b0(int *, int *) {}


template<typename T>
void v1(T, int *, int *) {}

#pragma omp declare variant(v1) match(construct={dispatch}) \
				adjust_args(need_device_ptr: V:omp_num_args)
template<int V>
void b1(S1<V>, int *, int *) {}


template<typename T>
void v2(T, int *, int *, int *) {}

#pragma omp declare variant(v2) match(construct={dispatch}) \
				adjust_args(need_device_ptr: LB:UB)
template<int LB, int UB>
void b2(S2<LB, UB>, int *, int *, int *) {}


void f(int *p0, int *p1, int *p2, int *p3, int *p4, int *p5, int *p6)
{
  #pragma omp dispatch
  b0(p0, p1);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p1, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "v0 \\(p0, D\.\[0-9\]+\\);" "gimple" } }  */
  #pragma omp dispatch
  b1(S1<3>(), p2, p3);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p3, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "v1<S1<3> > \\(D\.\[0-9\]+, p2, D\.\[0-9\]+\\);" "gimple" } }  */
  #pragma omp dispatch
  b2(S2<2, 3>(), p4, p5, p6);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p4, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p5, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "v2<S2<2, 3> > \\(D\.\[0-9\]+, D\.\[0-9\]+, D\.\[0-9\]+, p6\\);" "gimple" } }  */
}
/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_default_device \\(\\);" 3 "gimple" } }  */
