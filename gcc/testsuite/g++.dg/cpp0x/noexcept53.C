// PR c++/86476 - noexcept-specifier is a complete-class context
// { dg-do compile { target c++11 } }

struct S {
  void f1() noexcept(noexcept(fn()));
  void f2() noexcept(noexcept(fnx()));
  void fn();
  void fnx() noexcept;
};

void
S::f1() noexcept // { dg-error "different exception specifier" }
{
}

void
S::f2() // { dg-error "different exception specifier" }
{
}

struct S2 {
  void f1() noexcept(noexcept(nosuchfn())); // { dg-error "not declared in this scope" }
  void f2() noexcept(noexcept(nothere)); // { dg-error "not declared in this scope" }
  void f3() noexcept(noexcept(nosuchfn())) { } // { dg-error "not declared in this scope" }
  void f4() noexcept(noexcept(nothere)) { } // { dg-error "not declared in this scope" }
};
