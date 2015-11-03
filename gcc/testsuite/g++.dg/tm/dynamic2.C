// { dg-options "-fgnu-tm -std=c++14 -O2" }

void unsafe();
struct A {
  virtual void f() transaction_safe_dynamic;
};
struct B:A {
  void f() { unsafe(); }
};

void f() transaction_safe {
  B b;
  A& ar = b;
  // This is undefined behavior, we want to give an error with
  // devirtualization.
  ar.f();			// { dg-error "unsafe" }
}
