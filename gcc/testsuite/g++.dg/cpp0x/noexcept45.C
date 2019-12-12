// PR c++/86476 - noexcept-specifier is a complete-class context
// { dg-do compile { target c++11 } }

struct A
{
  virtual void f();
  virtual void g() noexcept;
  virtual void h() noexcept(false);
};

struct B : A
{
  void f() noexcept(true);
  void g() noexcept(true);
  void h() noexcept(true);
};

struct D : A
{
  void f() noexcept(false);
  void g() noexcept(false); // { dg-error "looser exception specification" }
  void h() noexcept(false);
};
