// { dg-do compile }
// Contributed by Martin Loewis <loewis at informatik dot hu-berlin dot de>
// PR c++/8856: Make sure template conversion operators are not parsed as
//   template names.

struct K {};
template <bool> struct K2 {};

template <class T> struct A {
  template <class U> operator U() { return U(); }
};

int main() {
  A<double> a;

  (void)a.operator int();
  (void)a.operator double();
  (void)a.operator K2<true>();
  (void)a.A<double>::operator int();
  (void)a.A<double>::operator double();
  (void)a.A<double>::operator K2<true>();

  (void)a.operator double<int>();             // { dg-error "not a template" }
  (void)a.operator K<int>();                  // { dg-error "not a template" }
  (void)a.A<double>::operator double<int>();  // { dg-error "not a template" }
  (void)a.A<double>::operator K<int>();       // { dg-error "not a template" }
}
