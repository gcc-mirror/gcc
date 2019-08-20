// PR c++/19732

struct A;
typedef int ~A; // { dg-error "13:declaration of .~ A. as non-function" }
struct B { 
  ~A(); // { dg-error "3:declaration of .~A. as member of .B." }
  typedef int ~A; // { dg-error "15:declaration of .~ A. as non-function" }
  void f() {
    extern ~B(); // { dg-error "12:declaration of .~ B. as non-member" }
  }
};
void ~A(); // { dg-error "6:declaration of .~ A. as non-member" }
