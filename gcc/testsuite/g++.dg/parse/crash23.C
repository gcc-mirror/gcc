// PR c++/19733

struct A {};
typedef int I;
void foo() {
  A().~A; // { dg-error "" }
  A().A::~A; // { dg-error "" }
  (int().I::~I, 3); // { dg-error "" }
  int().I::~I; // { dg-error "" }
}

  
