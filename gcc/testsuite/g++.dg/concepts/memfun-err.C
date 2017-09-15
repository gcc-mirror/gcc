// { dg-do run}
// { dg-options "-std=c++17 -fconcepts" }


template<typename T>
  concept bool C() { return __is_class(T); }

template<typename T>
  concept bool D() { return __is_empty(T); }

struct X { } x;
struct Y { int n; } y;

int called = 0;

// Test constrained member definitions
template<typename T>
  struct S1 { // { dg-message "defined here" }
    void f1() requires C<T>() { }
    void g1() requires C<T>() and true;
    template<C U> void h1(U u) { called = 1; }

    void g2() requires C<T>(); // { dg-message "candidate" }
  };

template<typename T>
  void S1<T>::g2() requires D<T>() { } // { dg-error "no declaration matches" }

int main() {
  S1<X> sx;
  S1<Y> sy;
  S1<int> si;

  si.f1(); // { dg-error "matching" }
  si.g1(); // { dg-error "matching" }
  si.h1(0); // { dg-error "matching" }
}
