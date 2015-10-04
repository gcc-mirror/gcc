// Test that transaction_safe_dynamic can only be used on virtual functions.
// { dg-options "-fgnu-tm -std=c++14" }

void f() transaction_safe_dynamic; // { dg-error "virtual" }
auto a = []() transaction_safe_dynamic {}; // { dg-error "virtual" }
struct A {
  void f() transaction_safe_dynamic; // { dg-error "virtual" }
  virtual void g();
};

struct B: A {
  void g() transaction_safe_dynamic;
};
