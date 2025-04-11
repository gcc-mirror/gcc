// check that we do not get unused warnings for contract check function parameters
// { dg-do compile }
// { dg-options "-std=c++2b -fcontracts -fcontracts-nonattr -fcontracts-on-virtual-functions=P3653" }


void foo() pre inherited(Base); // { dg-error "inherited contracts are only available on member functions" }

struct Base
{
  virtual void f() pre (true){};
};

struct Bob;
struct Child0 : Base
{
  virtual void f() pre inherited(Base,); // { dg-error "expected class-name at end of input" }
  virtual void g1() pre inherited(Base); // { dg-error "does not override" }
  virtual void g2() pre inherited(Bob); // { dg-error "contracts can only be inherited from a direct base class" }
  virtual void g3() pre inherited(int); // { dg-error "expected" }

  void h() pre inherited(Base);// { dg-error "contracts can only appear on a virtual function" }
};

struct GChild: Child0
{
  virtual void f() pre inherited(Base); // { dg-error "contracts can only be inherited from a direct base class" }
};
