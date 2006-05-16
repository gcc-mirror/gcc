// DR 401

class X {
  typedef int a; // { dg-error "private" }
  static const int b = 5; // { dg-error "private" }
  template <typename>
  struct c; // { dg-error "private" }
};

template <typename = X::a> // { dg-error "context" }
struct A;

template <int = X::b> // { dg-error "context" }
struct B;

template <template <typename> class T = X::c> // { dg-error "context" }
struct C;
  
  
