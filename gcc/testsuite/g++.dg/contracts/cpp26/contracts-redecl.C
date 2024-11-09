// generic error tests for generalized contract redecls
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr" }

// allowed to repeat contracts or omit them
int g0(int a) pre (a > 0);
int g0(int a) pre (a > 0 );
int g0(int a) pre (a > 0);

int g1(int a) pre (a > 0);
int g1(int a);

int g2(int a);
int g2(int a) pre (a > 0); // { dg-error "declaration adds contracts" }

struct G0
{
  int f(int a);
};

int G0::f(int a) pre (a > 0) // { dg-error "declaration adds contracts" }
{
  return -a;
}

struct G1
{
  int f(int a) pre (a > 0);
};

int G1::f(int a)  pre ( a > 0 );
int G1::f(int a);
int G1::f(int a)  pre ( a > 0 ) pre ( a > 0 ); // { dg-error "different number of contracts" }
int G1::f(int b) pre ( b > 0 ); // OK. same as the first decl.



int f0(int a) pre ( a > 0 );
int f0(int a) pre ( a > 0 )  pre ( a > 10 ); // { dg-error "different number of contracts" }

int f1(int a) pre ( a > 0 );
int f1(int a) pre ( a < 0 ); // { dg-error "mismatched contract" }

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


struct T1
{
  void vfun(int m, double n);
};

void T1::vfun(int m, double n)
   pre ( x < 0 ) // { dg-error "was not declared in this" }
{
}

void T1::vfun(int m, double n) pre(true); // { dg-error "declaration adds contracts" }

struct Foo {
  virtual void f10 (int)  pre ( false ) {}
};

struct Bar : Foo {
  void f10 (int n = 0) override pre ( false );
};

// we currently don't diagnose an error here if the original contract was erroneous.
void Bar::f10(int n) pre (n >10) {}; // { dg-error "mismatched contract" }


