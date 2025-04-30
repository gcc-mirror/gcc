/* { dg-require-effective-target c++11 } */
/* PR c++/118859 */

/* Diagnose invalid substituted types of depdendent parameters specified
   in a need_device_ptr/need_device_addr modified adjust_args clause.
   TODO: Need more cases with varying reference specifiers in the
   variant/base function.  */

template<typename T>
void v0(T) {}

#pragma omp declare variant(v0) match(construct={dispatch}) \
				adjust_args(need_device_ptr: 1) /* { dg-note "parameter specified here" } */
template<typename T>
void b0(T) {} /* { dg-error "parameter specified in an 'adjust_args' clause with the 'need_device_ptr' modifier must be of pointer type" } */


template<typename T>
void v1(T) {}

#pragma omp declare variant(v1) match(construct={dispatch}) \
				adjust_args(need_device_ptr: 1)
template<typename T>
void b1(T) {}

void f0(int *p0, int *p1)
{
  #pragma omp dispatch
  b0(42); /* { dg-message "required from here" } */
  #pragma omp dispatch
  b0(p0); /* { dg-bogus "required from here" } */
  #pragma omp dispatch
  b1(p1); /* { dg-bogus "required from here" } */
}

template<typename T>
struct Type { typedef T type; };

template<typename T>
void v2(Type<T>, typename Type<T>::type) {}

#pragma omp declare variant(v2) match(construct={dispatch}) \
				adjust_args(need_device_addr: 2) /* { dg-note "parameter specified here" } */
template<typename T>
void b2(Type<T>, typename Type<T>::type) {} /* { dg-error "parameter specified in an 'adjust_args' clause with the 'need_device_addr' modifier must be of reference type" } */


template<typename T>
void v3(Type<T>, typename Type<T>::type) {}

#pragma omp declare variant(v3) match(construct={dispatch}) \
				adjust_args(need_device_addr: 2)
template<typename T>
void b3(Type<T>, typename Type<T>::type) {}


void f2(int &r0, int &r1)
{
  #pragma omp dispatch
  b2(Type<int>(), 42); /* { dg-message "required from here" } */
  #pragma omp dispatch
  b2(Type<int&>(), r0); /* { dg-bogus "required from here" } */
  #pragma omp dispatch
  b3(Type<int&>(), r1); /* { dg-bogus "required from here" } */
}

template<typename T>
void vX(T) {}

#pragma omp declare variant(vX) match(construct={dispatch}) \
				adjust_args(need_device_ptr: 1)
template<typename T>
void bX(T) {}

template<typename T>
void vY(T&) {}

#pragma omp declare variant(vY) match(construct={dispatch}) \
				adjust_args(need_device_ptr: 1) /* { dg-note "parameter specified here" } */
template<typename T>
void bY(T&) {} /* { dg-message "parameter with type reference to pointer in an 'adjust_args' with the 'need_device_ptr' modifier is not currently supported" } */


template<typename T>
void v4(Type<T>, typename Type<T>::type) {}

#pragma omp declare variant(v4) match(construct={dispatch}) \
				adjust_args(need_device_ptr: 2) /* { dg-note "parameter specified here" } */
template<typename T>
void b4(Type<T>, typename Type<T>::type) {} /* { dg-message "parameter with type reference to pointer in an 'adjust_args' with the 'need_device_ptr' modifier is not currently supported" } */


void f3(int *p, int *&rp0, int *&rp1)
{
  #pragma omp dispatch
  bX(rp0); /* { dg-bogus "required from here" } */
  #pragma omp dispatch
  bY(p); /* { dg-message "required from here" } */
  #pragma omp dispatch
  b4(Type<int *&>(), rp1); /* { dg-message "required from here" } */
}
