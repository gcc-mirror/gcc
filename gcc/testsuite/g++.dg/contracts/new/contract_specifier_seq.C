// generic assert contract parsing checks
//   check omitted, 'default', 'audit', and 'axiom' contract levels parse
//   check that all concrete semantics parse
//   check omitted, '%default' contract roles parse
//   ensure that an invalid contract level 'invalid' errors
//   ensure that a predicate referencing an undefined variable errors
//   ensure that a missing colon after contract level errors
//   ensure that an invalid contract role 'invalid' errors
//   ensure that a missing colon after contract role errors
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr" }

// one erroneous contract
int a() pre(f(3) > 2);  // { dg-error "was not declared" }

// erroneous contract in the middle
int b(int i)
  pre(true)
  pre(f(3) > 2) // { dg-error "was not declared" "" { target *-*-* } }
  post(true);

void c(int i)
  post(true) -> int ; // { dg-error "expected initializer" }


void f1(int i) pre(true) [[pre:i>0]]; // { dg-error "was not declared" }
void f2(int i) pre(k>0) [[pre:i>0]]; // { dg-error "was not declared" }
void f3(int i) pre(true [[pre:i>0]]); // { dg-error "was not declared" }
// { dg-error "shall only introduce an attribute" "" { target *-*-* } .-1 }
// { dg-error "expected" "" { target *-*-* } .-2 }

struct B{

  virtual void f() post(true) = 0;

};

struct D : B {
  void f() override post(true) = 0;
};

// non attribute contracts come after override.
struct E : D {
  void f() post(true) override;  // { dg-error "expected" }
  // { dg-error "override" "" { target *-*-* } .-1 }
};




int main()
{
}
