/* { dg-do compile { target c++11 } } */

/* Invalid value constexpr var/NTTP in numeric range in functions
   with a parameter pack.
   TODO: Add more cases.  */

template<int>
struct S1 {};

const int G0 = 0;
const int G1 = 2;


template<typename... Ts>
void v_g0(int *, Ts...) {}
template<typename... Ts>
void v_g1(int *, Ts...) {}
template<typename... Ts>
void v_g2(int *, Ts...) {}


/* Always invalid, should diagnose without instantiation.  */
/* { dg-error "expression of bound must be positive" "" { target *-*-* } .+2 } */
#pragma omp declare variant(v_g0) match(construct={dispatch}) \
				  adjust_args(need_device_ptr: G0+0:omp_num_args)
template<typename... Ts>
void b_g0(int *, Ts...) {}

#pragma omp declare variant(v_g1) match(construct={dispatch}) \
				  adjust_args(need_device_ptr: G1+0:omp_num_args)
template<typename... Ts>
void b_g1(int *, Ts...) {}

/* { dg-error "numeric range lower bound must be less than or equal to upper bound" "" { target *-*-* } .+2 } */
#pragma omp declare variant(v_g2) match(construct={dispatch}) \
				  adjust_args(need_device_ptr: G1+0:omp_num_args)
template<typename... Ts>
void b_g2(int *, Ts...) {}


void f0 (int *p0)
{
  /* All 3 of these have a fixed lb, equal to G1.  */
  /* In range, no error.  */
  #pragma omp dispatch
  b_g1(p0, p0, p0);
  #pragma omp dispatch
  b_g2(p0, p0, p0);
  /* Out of range.  */
  #pragma omp dispatch
  b_g2(p0);  /* { dg-message "required from here" } */
}


template<typename T, typename... Ts>
void v0(T, Ts...) {}

/* { dg-error "expression of bound must be positive" "" { target *-*-* } .+2 } */
#pragma omp declare variant(v0) match(construct={dispatch}) \
				adjust_args(need_device_ptr: V+0:omp_num_args)
template<int V, typename... Ts>
void b0(S1<V>, Ts...) {}

template<typename T, typename... Ts>
void v1(T, Ts...) {}

#pragma omp declare variant(v1) match(construct={dispatch}) \
				adjust_args(need_device_ptr: V+0:omp_num_args)
template<int V, typename... Ts>
void b1(S1<V>, Ts...) {}

template<typename T, typename... Ts>
void v2(T, Ts...) {}

/* { dg-error "expression of bound is out of range" "" { target *-*-* } .+2 } */
#pragma omp declare variant(v2) match(construct={dispatch}) \
				adjust_args(need_device_ptr: V+0:omp_num_args)
template<int V, typename... Ts>
void b2(S1<V>, Ts...) {}

void f1(int *p0, int *p1, int *p2)
{
  #pragma omp dispatch
  b0(S1<0>{}, p0, p1, p2);  /* { dg-message "required from here" } */
  /* In range, no error.  */
  #pragma omp dispatch
  b1(S1<2>{}, p0); /* { dg-bogus "required from here" } */
  #pragma omp dispatch
  b1(S1<4>{}, p0, p1, p2); /* { dg-bogus "required from here" } */
  /* Out of range.  */
  #pragma omp dispatch
  b2(S1<3>{}, p0); /* { dg-message "required from here" } */
  #pragma omp dispatch
  b2(S1<5>{}, p0, p1, p2); /* { dg-message "required from here" } */
}
