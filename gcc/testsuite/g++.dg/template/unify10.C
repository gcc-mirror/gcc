// { dg-do compile }
// Origin: Wolfgang Bangerth <bangerth at ticam dot utexas dot edu>
//     and Rene Fonseca <fonseca at mip dot sdu dot dk>
// PR c++/8271: Check cv-qualifiers while unifying pointer to member
//  functions.

struct MyClass {
  void mMethod() throw() {}
  void cMethod() const throw() {}
  void vMethod() volatile throw() {}
  void cvMethod() const volatile throw() {}
};

template<class CLASS>
void mFunction(void (CLASS::* method)()) {} // { dg-message "note" }

template<class CLASS>
void cFunction(void (CLASS::* method)() const) {} // { dg-message "note" }

template<class CLASS>
void vFunction(void (CLASS::* method)() volatile) {} // { dg-message "note" }

template<class CLASS>
void cvFunction(void (CLASS::* method)() const volatile) {} // { dg-message "note" }

int main() {
  mFunction(&MyClass::mMethod);
  mFunction(&MyClass::cMethod);    // { dg-error "no matching function" }
  // { dg-message "(candidate|incompatible cv-qualifiers)" "candidate note" { target *-*-* } .-1 }
  mFunction(&MyClass::vMethod);    // { dg-error "no matching function" }
  // { dg-message "(candidate|incompatible cv-qualifiers)" "candidate note" { target *-*-* } .-1 }
  mFunction(&MyClass::cvMethod);   // { dg-error "no matching function" }
  // { dg-message "(candidate|incompatible cv-qualifiers)" "candidate note" { target *-*-* } .-1 }

  cFunction(&MyClass::mMethod);    // { dg-error "no matching function" }
  // { dg-message "(candidate|incompatible cv-qualifiers)" "candidate note" { target *-*-* } .-1 }
  cFunction(&MyClass::cMethod);
  cFunction(&MyClass::vMethod);    // { dg-error "no matching function" }
  // { dg-message "(candidate|incompatible cv-qualifiers)" "candidate note" { target *-*-* } .-1 }
  cFunction(&MyClass::cvMethod);   // { dg-error "no matching function" }
  // { dg-message "(candidate|incompatible cv-qualifiers)" "candidate note" { target *-*-* } .-1 }

  vFunction(&MyClass::mMethod);    // { dg-error "no matching function" }
  // { dg-message "(candidate|incompatible cv-qualifiers)" "candidate note" { target *-*-* } .-1 }
  vFunction(&MyClass::cMethod);    // { dg-error "no matching function" }
  // { dg-message "(candidate|incompatible cv-qualifiers)" "candidate note" { target *-*-* } .-1 }
  vFunction(&MyClass::vMethod);
  vFunction(&MyClass::cvMethod);   // { dg-error "no matching function" }
  // { dg-message "(candidate|incompatible cv-qualifiers)" "candidate note" { target *-*-* } .-1 }

  cvFunction(&MyClass::mMethod);   // { dg-error "no matching function" }
  // { dg-message "(candidate|incompatible cv-qualifiers)" "candidate note" { target *-*-* } .-1 }
  cvFunction(&MyClass::cMethod);   // { dg-error "no matching function" }
  // { dg-message "(candidate|incompatible cv-qualifiers)" "candidate note" { target *-*-* } .-1 }
  cvFunction(&MyClass::vMethod);   // { dg-error "no matching function" }
  // { dg-message "(candidate|incompatible cv-qualifiers)" "candidate note" { target *-*-* } .-1 }
  cvFunction(&MyClass::cvMethod);

  return 0;
}
