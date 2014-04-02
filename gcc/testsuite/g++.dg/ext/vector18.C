/* { dg-do compile { target c++11 } } */

typedef signed char __attribute__((vector_size(128) )) vec;

template <class A, class B>
auto f (A *a, B b) -> decltype (*a + b);

void f (...) {}

void g (vec *v, long long l)
{
  f (v, l);
}
