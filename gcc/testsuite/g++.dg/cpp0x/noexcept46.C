// PR c++/86476 - noexcept-specifier is a complete-class context
// { dg-do compile { target c++11 } }

void f() noexcept(false);
void g() noexcept(true);
void h() noexcept;

struct B {
  friend void f() noexcept(false);
  friend void g() noexcept(false); // { dg-error "different exception specifier" }
  friend void h() noexcept(false); // { dg-error "different exception specifier" }
};

struct C {
  friend void f() noexcept(true); // { dg-error "different exception specifier" }
  friend void g() noexcept(true); // { dg-error "different exception specifier" }
  friend void h() noexcept(true); // { dg-error "different exception specifier" }
};

void o() noexcept(false);
void p() noexcept(true);
void q() noexcept;

struct D {
  friend void o() noexcept(true); // { dg-error "different exception specifier" }
  friend void p() noexcept(true);
  friend void q() noexcept(true);
};
