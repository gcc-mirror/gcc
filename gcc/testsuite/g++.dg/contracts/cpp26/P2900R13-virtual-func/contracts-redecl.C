// generic error tests for generalized contract redecls
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr -fcontracts-nonattr-inheritance-mode=P2900R13" }


struct Base
{
  virtual int f(int a) pre (a > 0);
};

struct Child : Base
{
  int f(int a) pre (a < 0); // ok, does not inherit contracts
};

struct F1
{
  virtual int f(int a);
};

int F1::f(int a) pre (a > 0) // { dg-error "declaration adds contracts" }
{
  return -a;
}

struct Foo {
  virtual void f10 (int)  pre ( false ) {}
};

struct Bar : Foo {
  void f10 (int n = 0) override pre ( false );
};

// we currently don't diagnose an error here if the original contract was erroneous.
void Bar::f10(int n) pre (n >10) {}; // { dg-error "mismatched contract" }

template <typename T>
struct Base2
{
  virtual int f(const int a) pre( a  > 5 ) post( a > 7) { return a;};
  int g(const int a) pre( a  > 5 ) post( a > 7) { return a;};
  virtual int h(int a) pre( a  > 5 ) post( a > 7) { return a;}; // { dg-error "a value parameter used in a postcondition must be const" }
  int i(int a) pre( a  > 5 ) post( a > 7) { return a;}; // { dg-error "a value parameter used in a postcondition must be const" }
};
