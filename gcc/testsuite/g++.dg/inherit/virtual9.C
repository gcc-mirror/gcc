// { dg-do compile }
struct B
{
  virtual void f() final {}
  virtual void g() {}
};

struct B2
{
  virtual void h() {}
};

struct D : B
{
  virtual void g() override final {} // { dg-error "overriding" }
};

template <class T> struct D2 : T
{
  void h() override {} // { dg-error "marked override, but does not override" }
};

struct D3 : D
{
  void g() {} // { dg-error "virtual function" }
};

struct B3
{
  virtual void f() final final {} // { dg-error "duplicate virt-specifier" }
};

void g() override {} // { dg-error "virt-specifiers" }

int main()
{
  D2<B> d2;
  D2<B2> d3;
}
