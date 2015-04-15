// DR 401

class X {
  typedef int a; // { dg-message "private" }
  static const int b = 5; // { dg-message "private" }
  template <typename>
  struct c; // { dg-message "private" }
};

template <typename = X::a> // { dg-error "context" }
struct A;

template <int = X::b> // { dg-error "context" }
struct B;

template <template <typename> class T = X::c> // { dg-error "context" }
struct C;
  
  
