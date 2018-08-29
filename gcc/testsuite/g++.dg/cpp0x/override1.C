// { dg-do compile { target c++11 } }
struct B
{
  virtual void f() final {}
  virtual void g() {}
  virtual void x() const {}
  virtual void y() final;
};

void B::y() {} // { dg-message "overridden" }

struct B2
{
  virtual void h() {}
};

struct D : B
{
  virtual void g() override final {} // { dg-message "overridden" }
  virtual void y() override final {} // { dg-error "virtual" }
};

template <class T> struct D2 : T
{
  void h() override {} // { dg-error "marked 'override', but does not override" }
};

template <class T> struct D3 : T
{
  void h() override {}
};

struct D4 : D
{
  void g() {} // { dg-error "virtual function" }
};

struct B3
{
  virtual void f() final final {} // { dg-error "duplicate virt-specifier" }
};

struct B4
{
  void f() final {} // { dg-error "marked 'final', but is not virtual" }
};

struct D5 : B
{
  void ff() override {} // { dg-error "marked 'override', but does not override" }
  virtual void fff() override {} // { dg-error "marked 'override', but does not override" }
  virtual void x() override {} // { dg-error "marked 'override', but does not override" }
  void g() override;
};

void D5::g() override {} // { dg-error "not allowed outside a class definition" }
void g() override {} // { dg-error "not allowed outside a class definition" }

struct B5
{
  friend void f() final; // { dg-error "may not have virt-specifiers" }
  friend void g() override; // { dg-error "may not have virt-specifiers" }
  template <class T> void h() final; // { dg-error "may not have virt-specifiers" }
  template <class T> void i() override; // { dg-error "may not have virt-specifiers" }
};

int main()
{
  D2<B> d;
  D2<B2> d2;
  D3<B2> d3;
}
