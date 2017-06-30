// PR c++/31132

template<typename T> class A
{
  static int i; // { dg-message "private" }
  friend int T::foo(); // { dg-error "no declaration matches" }
};

struct B
{
  void foo() { A<B>::i; } // { dg-message "candidate" }
  // { dg-error "private within" "" { target *-*-* } .-1 }
};
