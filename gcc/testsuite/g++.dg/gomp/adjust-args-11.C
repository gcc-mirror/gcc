/* { dg-do compile { target c++11 } } */
/* { dg-additional-options "-fdump-tree-gimple" }  */

/* Valid constexpr variable/NTTP in numeric range in functions
   with a parameter pack.  */

template<int, int>
struct S2 {};

template<typename T, typename... Ts>
void v0(T, Ts...) {}

#pragma omp declare variant(v0) match(construct={dispatch}) \
				adjust_args(need_device_ptr: LB:UB)
template<int LB, int UB, typename... Ts>
void b0(S2<LB, UB>, Ts...) {}


void f0(int *p0, int *p1, int *p2)
{
  #pragma omp dispatch
  b0(S2<2, 3>{}, p0, p1, p2);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p0, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p1, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "v0<S2<2, 3>, int\\*, int\\*, int\\*> \\(D\.\[0-9\]+, D\.\[0-9\]+, D\.\[0-9\]+, p2\\);" "gimple" } }  */
}


/* With multiple instantiations.  */

template<typename T, typename... Ts>
void v1(T, Ts...) {}

#pragma omp declare variant(v1) match(construct={dispatch}) \
			       adjust_args(need_device_ptr: V0+0:V1+0)
template<int V0, int V1, typename... Ts>
void b1(S2<V0, V1>, Ts...) {}

void f1(int *f1_p0, int *f1_p1, int *f1_p2, int *f1_p3, int *f1_p4)
{
  #pragma omp dispatch
  b1(S2<3, 3>{}, f1_p0, f1_p1);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(f1_p1, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "v1<S2<3, 3>, int\\*, int\\*> \\(D\.\[0-9\]+, f1_p0, D\.\[0-9\]+\\);" "gimple" } }  */
  #pragma omp dispatch
  b1(S2<4, 4>{}, f1_p2, f1_p3, f1_p4);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(f1_p4, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "v1<S2<4, 4>, int\\*, int\\*, int\\*> \\(D\.\[0-9\]+, f1_p2, f1_p3, D\.\[0-9\]+\\);" "gimple" } }  */
}



/* Multiple instantiations of function with parameter packs
   with an adjust_args clause with a relative numeric range.  */

template<typename... Ts>
void v2(Ts...) {}

#pragma omp declare variant(v2) match(construct={dispatch}) \
				adjust_args(need_device_ptr: 1:omp_num_args)
template<typename... Ts>
void b2(Ts...) {}

void f2(int *f2_p0, int *f2_p1, int *f2_p2, int *f2_p3, int *f2_p4, int *f2_p5)
{
  #pragma omp dispatch
  b2(f2_p0);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(f2_p0, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "v2<int\\*> \\(D\.\[0-9\]+\\);" "gimple" } }  */
  #pragma omp dispatch
  b2(f2_p1, f2_p2);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(f2_p1, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(f2_p2, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "v2<int\\*, int\\*> \\(D\.\[0-9\]+, D\.\[0-9\]+\\);" "gimple" } }  */
  #pragma omp dispatch
  b2(f2_p3, f2_p4, f2_p5);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(f2_p3, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(f2_p4, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(f2_p5, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "v2<int\\*, int\\*, int\\*> \\(D\.\[0-9\]+, D\.\[0-9\]+, D\.\[0-9\]+\\);" "gimple" } }  */
}


template<typename... Ts>
void v3(Ts...) {}

#pragma omp declare variant(v3) match(construct={dispatch}) \
				adjust_args(need_device_ptr: 1:omp_num_args)
template<typename... Ts>
void b3(Ts...) {}

void f3(int *f3_p0, int *f3_p1, int *f3_p2, int *f3_p3, int *f3_p4, int *f3_p5)
{
  #pragma omp dispatch
  b3(f3_p0, f3_p1, f3_p2);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(f3_p0, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(f3_p1, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(f3_p2, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "v3<int\\*, int\\*, int\\*> \\(D\.\[0-9\]+, D\.\[0-9\]+, D\.\[0-9\]+\\);" "gimple" } }  */
  #pragma omp dispatch
  b3(f3_p3, f3_p4);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(f3_p3, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(f3_p4, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "v3<int\\*, int\\*> \\(D\.\[0-9\]+, D\.\[0-9\]+\\);" "gimple" } }  */
  #pragma omp dispatch
  b3(f3_p5);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(f3_p5, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "v3<int\\*> \\(D\.\[0-9\]+\\);" "gimple" } }  */
}


/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_default_device \\(\\);" 9 "gimple" } }  */
