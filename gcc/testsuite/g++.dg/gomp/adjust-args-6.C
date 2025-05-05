/* PR c++/119659 */

/* { dg-do compile { target c++11 } } */
/* { dg-additional-options "-fdump-tree-gimple" } */

/* Test correct argument gets adjusted.  */

struct S {
  template<typename... Ts>
  void v0_pack(int *, Ts...) {}

  #pragma omp declare variant(v0_pack) match(construct={dispatch}) \
				       adjust_args(need_device_ptr: a)
  template<typename... Ts>
  void b0_pack(int *a, Ts...) { static_cast<void>(a); }


  template<typename... Ts>
  void v1_pack(int *, Ts...) {}

  #pragma omp declare variant(v1_pack) match(construct={dispatch}) \
				       adjust_args(need_device_ptr: 1)
  template<typename... Ts>
  void b1_pack(int *a, Ts...) { static_cast<void>(a); }


  template<typename... Ts>
  void v2_pack(int *, Ts...) {}

  #pragma omp declare variant(v2_pack) match(construct={dispatch}) \
				       adjust_args(need_device_ptr: 1:1)
  template<typename... Ts>
  void b2_pack(int *a, Ts...) { static_cast<void>(a); }


  template<typename... Ts>
  void v3_pack(int *, Ts...) {}

  #pragma omp declare variant(v3_pack) match(construct={dispatch}) \
				       adjust_args(need_device_ptr: 2)
  template<typename... Ts>
  void b3_pack(int *a, Ts...) { static_cast<void>(a); }


  template<typename... Ts>
  void v4_pack(int *, Ts...) {}

  #pragma omp declare variant(v4_pack) match(construct={dispatch}) \
				       adjust_args(need_device_ptr: 2:2)
  template<typename... Ts>
  void b4_pack(int *a, Ts...) { static_cast<void>(a); }
};


void f(int *p0, int *p1, int *p2, int *p3,
       int *p4, int *p5, int *p6, int *p7,
       int *p8, int *p9, int *pA, int *pB,
       int *pC, int *pD, int *pE, int *pF)
{
  S s;
  #pragma omp dispatch
  s.b0_pack(p0, p1);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p0, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "S::v0_pack<int\\*> \\(&s, D\.\[0-9\]+, p1\\);" "gimple" } }  */

  #pragma omp dispatch
  s.b1_pack(p2, p3);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p2, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "S::v1_pack<int\\*> \\(&s, D\.\[0-9\]+, p3\\);" "gimple" } }  */

  #pragma omp dispatch
  s.b2_pack(p4, p5);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p4, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "S::v2_pack<int\\*> \\(&s, D\.\[0-9\]+, p5\\);" "gimple" } }  */

  #pragma omp dispatch
  s.b3_pack(p6, p7);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p7, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "S::v3_pack<int\\*> \\(&s, p6, D\.\[0-9\]+\\);" "gimple" } }  */

  #pragma omp dispatch
  s.b3_pack(p8, p9, pA);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(p9, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "S::v3_pack<int\\*, int\\*> \\(&s, p8, D\.\[0-9\]+, pA\\);" "gimple" } }  */

  #pragma omp dispatch
  s.b4_pack(pB, pC);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(pC, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "S::v4_pack<int\\*> \\(&s, pB, D\.\[0-9\]+\\);" "gimple" } }  */

  #pragma omp dispatch
  s.b4_pack(pD, pE, pF);
/* { dg-final { scan-tree-dump "D\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(pC, D\.\[0-9\]+\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump "S::v4_pack<int\\*, int\\*> \\(&s, pD, D\.\[0-9\]+, pF\\);" "gimple" } }  */
}

/* { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = __builtin_omp_get_default_device \\(\\);" 7 "gimple" } }  */
