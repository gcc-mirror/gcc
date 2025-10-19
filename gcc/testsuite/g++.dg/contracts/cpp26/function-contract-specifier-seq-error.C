// generic function-contract-specifier-seq parsing checks
// N5008
// function-contract-specifier-seq :
//  	function-contract-specifier function-contract-specifier-seq opt
// function-contract-specifier :
//	precondition-specifier
// 	postcondition-specifier
// precondition-specifier : pre attribute-specifier-seq opt ( conditional-expression )
// postcondition-specifier :
//	post attribute-specifier-seq opt ( result-name-introducer opt conditional-expression )
// { dg-do compile { target c++26 } }
// { dg-additional-options "-fcontracts" }

int f(int);
int g1(int a) pre(f(a) > a) pre (true) post(false) pre(f(1) > 4)
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
