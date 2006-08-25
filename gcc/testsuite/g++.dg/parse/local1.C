// PR c++/28056

void f1();

namespace N {
  void f2();
}

class C {
  static void f3();
};

void foo() { 
  void ::f1();  // { dg-error "qualified" }
  void N::f2(); // { dg-error "qualified" }
  void C::f3(); // { dg-error "qualified" }
  void ::f4();  // { dg-error "qualified" }
}
