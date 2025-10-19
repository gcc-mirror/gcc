// N5008 :
// A declaration D of a function or function template f that is not a first declaration shall have either no
// function-contract-specifier-seq or the same function-contract-specifier-seq (see below) as any first declaration
// F reachable from D.
// { dg-do compile { target c++23 } }
// { dg-additional-options "-fcontracts" }

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
  int f(int a)
    pre (a > 0);
};

int G1::f(int a)
  pre ( a > 0 ) // OK. same as the first decl.
{ return 0; }

int G1::f(int a); // OK not adding

int G1::f(int a) // { dg-error "different number of contracts" }
  pre ( a > 0 ) pre ( a > 0 );

int f0(int a)
  pre ( a > 0 );

int f0(int a) // { dg-error "different number of contracts" }
  pre ( a > 0 ) pre ( a > 10 ); 

int f1(int a)
  pre ( a > 0 );
int f1(int a)
  pre ( a < 0 ); // { dg-error "mismatched contract" }

struct Base
{
  virtual int f(int a) // { dg-error "contracts cannot be added to virtual functions" }
    pre (a > 0);
};

struct Child : Base
{
  int f(int a) // { dg-error "contracts cannot be added to virtual functions" }
    pre (a < 0);
};

struct F1
{
  virtual int f(int a);
};

int F1::f(int a) // { dg-error "declaration adds contracts" }
  pre (a > 0)
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

void T1::vfun(int m, double n)  // { dg-error "declaration adds contracts" }
  pre(true);

struct Foo {
  virtual void f10 (int) // { dg-error "contracts cannot be added to virtual functions" }
    pre ( false ) {}
};

struct Bar : Foo {
  void f10 (int n = 0) override  // { dg-error "contracts cannot be added to virtual functions" }
    pre ( false );
};

// we currently don't diagnose an error here if the original contract was erroneous.
void Bar::f10(int n)
 pre (n >10) {}; // { dg-error "mismatched contract" }


struct NonTrivial{
  NonTrivial(){};
  NonTrivial(const NonTrivial&){}
  ~NonTrivial(){};
  int x = 0;
};

void f(const NonTrivial s) pre(s.x >0);
void f(const NonTrivial g) {};
void f(const NonTrivial t) pre(t.x >0);

void g(const NonTrivial t, const NonTrivial t1) pre(t.x >0);
void g(const NonTrivial t, const NonTrivial t1) pre(t1.x >0); // { dg-error "mismatched contract" }
void g(const NonTrivial g, const NonTrivial t1) pre(g.x >0){};
void g(const NonTrivial t, const NonTrivial t1) pre(t1.x >0); // { dg-error "mismatched contract" }

template<typename T>
bool somefunc(const T t){ return true;}

void h(const NonTrivial t, const NonTrivial t1) pre(somefunc(t) == true){};
void h(const NonTrivial t, const NonTrivial t1) pre(somefunc(t) == true);
void h(const NonTrivial t, const NonTrivial t1) pre(somefunc(t1) == true); // { dg-error "mismatched contract" }

double sqrt(const double x)
  post( r : r >= 0 );


double sqrt(const double x)
  post( r : r >= 0)
{
  return x;
}
