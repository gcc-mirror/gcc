// PR c++/31132

template<typename T> class A
{
  static int i; // { dg-message "private" }
  friend int T::foo(); // { dg-error "does not match" }
};

struct B
{
  void foo() { A<B>::i; } // { dg-error "within|candidate" }
};
