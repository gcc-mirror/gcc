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
// { dg-options "-std=c++23 -fcontracts -fcontracts-nonattr" }
struct A {
  A(A &);
};
struct S{
  void f(A a) pre(a) // { dg-error "could not convert" }
              pre(a.b) // { dg-error "has no member" }
  {

  }
};
void f(A a) pre(a) // { dg-error "could not convert" }
	    pre(a.b) // { dg-error "has no member" }
	    {
  contract_assert(a); // { dg-error "could not convert" }
  contract_assert(a.b); // { dg-error "has no member" }
}

int main()
{
}




