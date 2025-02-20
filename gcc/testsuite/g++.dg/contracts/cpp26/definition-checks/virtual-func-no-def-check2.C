// check that an invocation of a virtual function through the base class checks
// the base class contracts when definition side contracts are turned off
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr -fcontracts-nonattr-definition-check=off -fcontract-continuation-mode=on" }

struct Base
{
  virtual int f(const int a) pre (a > 14) post(r:r > 2){ return 0;};
};

struct Child : Base
{
  virtual int f(const int a) pre (a > 14) post(r:r > 2){ return 1;}
};

int fooBase(Base& b)
{
    return b.f(1);
}

int main(int, char**)
{
  Base b;
  Child c;

  fooBase (c);

  return 0;
}
// { dg-output "contract violation in function Base::f at .*: a > 14.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function Base::f at .*: r > 2.*(\n|\r\n|\r)" }
