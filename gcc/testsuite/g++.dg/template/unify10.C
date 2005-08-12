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
void mFunction(void (CLASS::* method)()) {}

template<class CLASS>
void cFunction(void (CLASS::* method)() const) {}

template<class CLASS>
void vFunction(void (CLASS::* method)() volatile) {}

template<class CLASS>
void cvFunction(void (CLASS::* method)() const volatile) {}

int main() {
  mFunction(&MyClass::mMethod);
  mFunction(&MyClass::cMethod);    // { dg-error "no matching function" }
  mFunction(&MyClass::vMethod);    // { dg-error "no matching function" }
  mFunction(&MyClass::cvMethod);   // { dg-error "no matching function" }

  cFunction(&MyClass::mMethod);    // { dg-error "no matching function" }
  cFunction(&MyClass::cMethod);
  cFunction(&MyClass::vMethod);    // { dg-error "no matching function" }
  cFunction(&MyClass::cvMethod);   // { dg-error "no matching function" }

  vFunction(&MyClass::mMethod);    // { dg-error "no matching function" }
  vFunction(&MyClass::cMethod);    // { dg-error "no matching function" }
  vFunction(&MyClass::vMethod);
  vFunction(&MyClass::cvMethod);   // { dg-error "no matching function" }

  cvFunction(&MyClass::mMethod);   // { dg-error "no matching function" }
  cvFunction(&MyClass::cMethod);   // { dg-error "no matching function" }
  cvFunction(&MyClass::vMethod);   // { dg-error "no matching function" }
  cvFunction(&MyClass::cvMethod);

  return 0;
}
