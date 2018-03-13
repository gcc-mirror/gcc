// Out-of-line generic member function definitions.
// { dg-do compile { target c++14 } }
// { dg-additional-options "-fconcepts" }

struct A {
  void f(auto x);
};

void A::f(auto x) {}  // injects a new list

template <typename T>
struct B {
  void f(auto x);
};

template <typename T>
void B<T>::f(auto x) {}  // injects a new list

struct C {
  template <int N>
  void f(auto x);
};

template <int N>
void C::f(auto x) {}  // extends existing inner list

template <typename T>
struct D
{
  template <int N>
  void f(auto x);
};

template <typename T>
template <int N>
void D<T>::f(auto x) {}  // extends existing inner list
