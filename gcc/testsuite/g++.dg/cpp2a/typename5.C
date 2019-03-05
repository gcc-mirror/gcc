// P0634R3
// { dg-do compile { target c++2a } }

struct X {
  template<typename T>
  struct N { };
};

template <typename T>
struct Y {
  template<typename U>
  struct N { };
};

template <typename T>
struct A
{
  template <typename U>
  struct N { };

  typedef typename A::template N<int> a1;
  typedef typename A::template N<T> a2;
  typename A::template N<int> a3;
  typename A::template N<T> a4;
  A::template N<int> a9;
  A::template N<T> a10;
  typedef A<T>::template N<int> a13;
  typedef A<T>::template N<T> a14;

  typedef typename X::template N<int> x1;
  typedef typename X::template N<T> x2;
  typename X::template N<int> x3;
  typename X::template N<T> x4;
  typedef X::N<int> x5;
  typedef X::N<T> x6;
  typedef typename X::N<int> x7;
  typedef typename X::N<T> x8;
  X::N<int> x9;
  X::N<T> x10;
  typename X::N<int> x11;
  typename X::N<T> x12;

  typedef typename Y<int>::template N<int> y1;
  typedef typename Y<int>::template N<T> y2;
  typedef typename Y<T>::template N<int> y3;
  typedef typename Y<T>::template N<T> y4;
  typename Y<int>::template N<int> y5;
  typename Y<int>::template N<T> y6;
  typename Y<T>::template N<int> y7;
  typename Y<T>::template N<T> y8;
  typedef Y<int>::N<int> y9;
  typedef Y<int>::N<T> y10;
  typedef Y<T>::template N<int> y11;
  typedef Y<T>::template N<T> y12;
  typedef typename Y<int>::N<int> y13;
  typedef typename Y<int>::N<T> y14;
  Y<int>::N<int> y17;
  Y<int>::N<T> y18;
  typename Y<int>::N<int> y21;
  typename Y<int>::N<T> y22;
  typedef Y<int>::N<int> y25;
  typedef Y<int>::N<T> y26;
  typedef Y<T>::template N<int> y27;
  typedef Y<T>::template N<T> y28;
};
