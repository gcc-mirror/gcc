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
 // { dg-error {expected .;. before '=' token} "" { target *-*-* } .-1 }
 // { dg-error {expected primary-expression before '=' token} "" { target *-*-* } .-2 }

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
		  
