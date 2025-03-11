// generic assert contract parsing checks

// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr" }

static_assert (__cpp_contracts >= 201906);
static_assert (__cpp_contracts_literal_semantics >= 201906);
static_assert (__cpp_contracts_roles >= 201906);
int f(int);
int g1(int a) pre(f(a) > a)
{
	int r = a - f(a);
	return 2 * r;
}

int fun(int n)  pre (n > 0 );
void fun2(int n)  pre  n > 0 ; // { dg-error {expected '\(' before } }
 // { dg-error {expected '\)' before ';' token} "" { target *-*-* } .-1 }
void fun2(int n)  pre (: n > 0 ]]; // { dg-error  }

int main()
{
  int x;

  contract_assert ( x >= 0 );

  contract_assert ( x > 0 ? true : false );
  contract_assert ( x < 0 ? true : false );

  contract_assert ( x = 0 ); // { dg-error {expected '\)' before '=' token} }
 // { dg-error {expected semicolon before '=' token} "" { target *-*-* } .-1 }
 // { dg-error {expected primary-expression before '=' token} "" { target *-*-* } .-2 }
 // { dg-error {contract assertion on a non empty statement} "" { target *-*-* } .-3 }

  contract_assert ( y == 0 ); // { dg-error ".y. was not declared in this scope" }

  return 0;
}

struct Baz
{
  void f(int x) pre(x = 0); // { dg-error "expected conditional-expression" }
  void g(int x) pre(x, x); // { dg-error "expected conditional-expression" }
  void h(const int x) post(x = 0); // { dg-error "expected conditional-expression" }
  void i(const int x) post(x, x); // { dg-error "expected conditional-expression" }
};
		  
void postcond(int x) post(x >= 0); // { dg-error "a value parameter used in a postcondition must be const" }

struct PostCond {
  void postcond(int x) post(x >= 0); // { dg-error "a value parameter used in a postcondition must be const" }
  template<class T> void postcond2(T x) post(x >= 0);
};

template <class T> void
postcond3(T x) post(x >= 0);

void postcond4(const int y, int x) post(x >= 0); // { dg-error "a value parameter used in a postcondition must be const" }

void postcond5(int y, const int x) post(x >= 0);

template <typename T>
struct Base
{
  virtual int f(const int a) pre( a  > 5 ) post( a > 7) { return a;};
  int g(const int a) pre( a  > 5 ) post( a > 7) { return a;};
  virtual int h(int a) pre( a  > 5 ) post( a > 7) { return a;}; // { dg-error "a value parameter used in a postcondition must be const" }
  int i(int a) pre( a  > 5 ) post( a > 7) { return a;}; // { dg-error "a value parameter used in a postcondition must be const" }
};


void postcond6()
{
  Base<int> b;
  b.f(6);
  b.g(6);
  b.h(6);
  b.i(6);
}
		  
template<class T>
void
PostCond::postcond2(T x) post (x >= 0)  // { dg-error "a value parameter used in a postcondition must be const" }
{ x; }

template <class T>
void
postcond3(T x) post(x >= 0)
{ } 

void postcond7()
{
  PostCond p;
  p.postcond2 (2);
  postcond3 (4); // { dg-error "a value parameter used in a postcondition must be const" "" {target *-*-* } 56 }

}
