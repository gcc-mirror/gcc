// CWG2596
// { dg-do compile { target c++20 } }

struct Base {};

template<int N>
struct S : Base {
  friend int foo(Base&) requires (N == 1); // { dg-error "must be a definition" }
  friend int foo(Base&) requires (N == 2); // { dg-error "must be a definition" }

  template <class T>
  friend int bar(Base&) requires (N == 1); // { dg-error "must be a definition" }
  template <class T>
  friend int bar(Base&) requires (N == 2); // { dg-error "must be a definition" }
};
