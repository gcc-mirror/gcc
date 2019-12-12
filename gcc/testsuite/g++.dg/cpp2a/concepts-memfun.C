// { dg-do run { target c++2a } }

#include <cassert>

template<typename T>
  concept C = __is_class(T);

template<typename T>
  concept D = __is_empty(T);

struct X { } x;
struct Y { int n; } y;

int called = 0;

// Test constrained member definitions
template<typename T>
  struct S1 {
    void f1() requires C<T> { }

    void f2() requires C<T> { called = 1; }
    void f2() requires (not C<T>) { called = 2; }

    void f3() { called = 1; }
    void f3() requires C<T> { called = 2; }
    void f3() requires C<T> and D<T> { called = 3; }

    void g1() requires C<T> and true;

    void g2() requires C<T>;
    void g2() requires (not C<T>);

    void g3();
    void g3() requires C<T>;
    void g3() requires C<T> and D<T>;

    template<C U> void h1(U u) { called = 1; }
    template<C U> void h2(U u);
    template<C U> void h3(U u) requires D<U>;
  };

template<C T>
  struct S2 {
    void f(T) requires D<T>;
  };


int main() {
  S1<X> sx;
  S1<Y> sy;
  S1<int> si;

  // Defined in-class
  sx.f1();
  sx.f2(); assert(called == 1);
  sx.f3(); assert(called == 3);

  sy.f1();
  sy.f2(); assert(called == 1);
  sy.f3(); assert(called == 2);

  si.f2(); assert(called == 2);
  si.f3(); assert(called == 1);

  // Member function template tests
  S1<int> s1i;
  s1i.h1(x); assert(called == 1);
  s1i.h2(x); assert(called == 2);
  s1i.h3(x); assert(called == 3);

  // Defined out of class.
  sx.g1();
  sx.g2(); assert(called == 1);
  sx.g3(); assert(called == 3);

  sy.g1();
  sy.g2(); assert(called == 1);
  sy.g3(); assert(called == 2);

  si.g2(); assert(called == 2);
  si.g3(); assert(called == 1);
}

template<typename T>
  void S1<T>::g1() requires C<T> and true { }

template<typename T>
  void S1<T>::g2() requires C<T> { called = 1; }

template<typename T>
  void S1<T>::g2() requires (not C<T>) { called = 2; }

template<typename T>
  void S1<T>::g3() { called = 1; }

template<typename T>
  void S1<T>::g3() requires C<T> { called = 2; }

template<typename T>
  void S1<T>::g3() requires C<T> and D<T> { called = 3; }

template<typename T>
  template<C U>
    void S1<T>::h2(U u) { called = 2; }

template<typename T>
  template<C U>
      void S1<T>::h3(U u) requires D<U> { called = 3; }

template<C T>
  void S2<T>::f(T t) requires D<T> { called = 4; }
