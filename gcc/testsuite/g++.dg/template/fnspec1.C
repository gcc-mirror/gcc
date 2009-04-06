// PR c++/35146

template <typename T> struct S {};

template <typename R> struct ref;
template <>           struct ref<double> { typedef double result; };

template <typename T>
void foo(typename ref<T>::result, S<T>*);
template <>
void foo(S<double>,               S<double>*); // { dg-error "does not match" }
template <>
void foo(double alpha,            S<double>* x)
{
  alpha; x;
}
