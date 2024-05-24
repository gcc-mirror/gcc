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

static_assert (__cpp_contracts >= 201906);
static_assert (__cpp_contracts_literal_semantics >= 201906);
static_assert (__cpp_contracts_roles >= 201906);
i
int fun(int n)  pre (n > 0 );

int main()
{
  int x() [[ pre fun(0) > 0 ]];
  int y() pre (fun(0) > 0);
  int z() [[ pre fun(0) > 0 ]] pre (fun(0) > 0);
  return 0;
}
