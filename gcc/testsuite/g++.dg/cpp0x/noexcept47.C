// PR c++/86476 - noexcept-specifier is a complete-class context
// { dg-do compile { target c++11 } }

int fn1 ();
int fn2 () noexcept;
int fn3 () noexcept;

void g() noexcept(noexcept (fn2()));

struct S1 {
  friend void g1() noexcept(noexcept(fn2()));
  friend void g1() noexcept(noexcept(fn1())); // { dg-error "different exception specifier" }
};

struct S2 {
  friend void g2() noexcept(noexcept(fn1()));
  friend void g2() noexcept(noexcept(fn1()));
  friend void g2() noexcept(noexcept(fn1()));
};

struct S3 {
  friend void g3() noexcept(noexcept(fn1()));
  friend void g3() noexcept(noexcept(fn3())); // { dg-error "different exception specifier" }
};

struct S4 {
  friend void g4() noexcept(noexcept(fn2()));
  friend void g4() noexcept(noexcept(fn3()));
};

struct S5 {
  friend void g() noexcept(noexcept(fn3()));
};

struct S6 {
  friend void g() noexcept(noexcept(fn1())); // { dg-error "different exception specifier" }
};

struct S7 {
  friend void gg() noexcept(noexcept(fn3()));
};

void gg() noexcept(noexcept(fn1())); // { dg-error "different exception specifier" }

struct S8 {
  friend void g8();
  friend void g8() noexcept(noexcept(fn2())); // { dg-error "different exception specifier" }
};

struct S9 {
  friend void g9();
  friend void g9() noexcept(noexcept(fn1()));
};

struct S10 {
  friend void g10() noexcept(noexcept(fn1()));
  friend void g10();
};

struct S11 {
  friend void g11() noexcept(noexcept(fn2()));
  friend void g11(); // { dg-error "different exception specifier" }
};

struct S12 {
  friend void g12() noexcept(false);
  friend void g12() noexcept(noexcept(fn2())); // { dg-error "different exception specifier" }
};

struct S13 {
  friend void g13() noexcept(false);
  friend void g13() noexcept(noexcept(fn1()));
};

struct S14 {
  friend void g14() noexcept(noexcept(fn1()));
  friend void g14() noexcept(false);
};

struct S15 {
  friend void g15() noexcept(noexcept(fn2()));
  friend void g15() noexcept(false); // { dg-error "different exception specifier" }
};
