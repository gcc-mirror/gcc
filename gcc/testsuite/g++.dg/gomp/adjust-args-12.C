/* Invalid value constexpr var/NTTP in numeric range in functions.
   TODO: Add more cases.  */

template<int>
struct S1 {};

const int G0 = 0;
const int G1 = 2;


void v_g0(int *) {}

/* { dg-error "expression of bound must be positive" "" { target *-*-* } .+2 } */
#pragma omp declare variant(v_g0) match(construct={dispatch}) \
				  adjust_args(need_device_ptr: G0+0:omp_num_args)
void b_g0(int *) {}

void v_g1(int *) {}

/* { dg-error "expression of bound is out of range" "" { target *-*-* } .+2 } */
#pragma omp declare variant(v_g1) match(construct={dispatch}) \
				  adjust_args(need_device_ptr: G1+0:omp_num_args)
void b_g1(int *) {}


template<typename T>
void v0(T, int *) {}

#pragma omp declare variant(v0) match(construct={dispatch}) \
				adjust_args(need_device_ptr: V+0:omp_num_args)
template<int V>
void b0(S1<V>, int *) {}

template<typename T>
void v1(T, int *) {}

/* { dg-error "expression of bound must be positive" "" { target *-*-* } .+2 } */
#pragma omp declare variant(v1) match(construct={dispatch}) \
				adjust_args(need_device_ptr: V+0:omp_num_args)
template<int V>
void b1(S1<V>, int *) {}

template<typename T>
void v2(T, int *) {}

/* { dg-error "expression of bound is out of range" "" { target *-*-* } .+2 } */
#pragma omp declare variant(v2) match(construct={dispatch}) \
				adjust_args(need_device_ptr: V+0:omp_num_args)
template<int V>
void b2(S1<V>, int *) {}

void f0(int *p0)
{
  /* Not out of range.  */
  #pragma omp dispatch
  b0(S1<2>(), p0); /* { dg-bogus "required from here" } */
  /* Out of range.  */
  #pragma omp dispatch
  b1(S1<0>(), p0); /* { dg-message "required from here" } */
  #pragma omp dispatch
  b2(S1<3>(), p0); /* { dg-message "required from here" } */
}
