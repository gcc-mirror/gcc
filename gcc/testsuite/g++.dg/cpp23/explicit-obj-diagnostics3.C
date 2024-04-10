// P0847R7
// { dg-do compile { target c++23 } }

// rejection and diagnosis of an xobj parameter declared with a default argument

struct S {
  void f0(this S = {}) {} // { dg-error "an explicit object parameter may not have a default argument" }
  void f1(this S = {}); // { dg-error "an explicit object parameter may not have a default argument" }
  void f2(this S);
  void f10(this S s = {}) {} // { dg-error "an explicit object parameter may not have a default argument" }
  void f11(this S s = {}); // { dg-error "an explicit object parameter may not have a default argument" }
  void f12(this S s);
};

void S::f1(this S) {}
void S::f2(this S = {}) {} // { dg-error "an explicit object parameter may not have a default argument" }

void S::f11(this S s) {}
void S::f12(this S s = {}) {} // { dg-error "an explicit object parameter may not have a default argument" }

