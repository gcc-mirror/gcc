// PR c++/44587
// { dg-do run }

template <const char *N> struct A { static const char *p; };
template <const char *N> const char *A<N>::p = N;
template <class T> struct B { static const char c[1]; typedef A<B<T>::c> C; };
template <class T> const char B<T>::c[1] = "";
template <class T> struct D { static const char c[1]; typedef A<c> C; };
template <class T> const char D<T>::c[1] = "";

template <int& I> struct E { static int *ip; };
template <int& I> int* E<I>::ip = &I;
template <class T> struct F { static int i; typedef E<F<T>::i> C; };
template <class T> int F<T>::i;
template <class T> struct G { static int i; typedef E<i> C; };
template <class T> int G<T>::i;

#define AS(X) if (!(X)) return 1;
int main()
{
  AS(B<int>::C::p == B<int>::c);
  AS(B<float>::C::p == B<float>::c);
  AS(B<float>::C::p != B<int>::c);
  AS(D<int>::C::p == D<int>::c);
  AS(D<float>::C::p == D<float>::c);
  AS(D<float>::C::p != D<int>::c);
}
