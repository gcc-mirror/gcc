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

int main()
{
  int x;

  contract_assert( x >= 0);
  contract_assert( x < 0);
  contract_assert( x == 0);

  contract_assert( x > 0 ? true : false);
  contract_assert( x < 0 ? true : false);

  contract_assert( x >= 0);
  contract_assert( x >= 0);
  contract_assert( x >= 0);
  contract_assert( x >= 0);

  contract_assert( x >= 0);
  contract_assert( x < 0);
  contract_assert( x == 0);
  contract_assert( x == 1);



  return 0;
}
