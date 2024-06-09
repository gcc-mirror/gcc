// DR 1351, Problems with implicitly-declared exception-specifications
// { dg-do compile { target c++11 } }

struct B {
  virtual void f() noexcept;
  virtual void g();
  virtual void h() noexcept = delete;
};

struct D: B {
  void f();                     // { dg-error "looser" }
  void g() noexcept;            // OK
  void h() = delete;            // OK
};
