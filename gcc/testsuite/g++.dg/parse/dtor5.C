// PR c++/19732

struct A;
typedef int ~A; // { dg-error "non-function" }
struct B { 
  ~A(); // { dg-error "" }
  typedef int ~A; // { dg-error "non-function" }
  void f() {
    extern ~B(); // { dg-error "non-member" }
  }
};
void ~A(); // { dg-error "non-member" }
