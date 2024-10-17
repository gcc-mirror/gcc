// generic assert contract parsing checks
//   check omitted, 'default', 'audit', and 'axiom' contract levels parse
//   check that all concrete semantics parse
//   check omitted, '%default' contract roles parse
//   ensure that an invalid contract level 'invalid' errors
//   ensure that a predicate referencing an undefined variable errors
//   ensure that a missing colon after contract level errors
//   ensure that an invalid contract role 'invalid' errors
//   ensure that a missing colon after contract role errors
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr -fcontract-continuation-mode=on" }
#include <iostream>
#include <contracts>

#define VERIFY_ASSERT(statement, asserts)  \
	{ \
		bool violation = false;\
		try{ \
			statement; \
		} catch(int &ex) { \
			violation = true; \
		} \
		if ((asserts && !violation) || (!(asserts) && violation)) __builtin_abort(); \
	} \

static_assert (__cpp_contracts >= 201906);
static_assert (__cpp_contracts_literal_semantics >= 201906);
static_assert (__cpp_contracts_roles >= 201906);

void handle_contract_violation(const std::contracts::contract_violation &violation) {
  std::cerr << "custom std::handle_contract_violation called:"
    << " " << violation.location().line()
    << " " << violation.location().file_name()
    << std::endl;
  throw -(int)violation.location().line();
}

void foo(int x) pre (x>10){};

int main()
{
  VERIFY_ASSERT([[assert: false]], true);
  VERIFY_ASSERT([[assert: true]], false);

  VERIFY_ASSERT(contract_assert(true), false);
  VERIFY_ASSERT(contract_assert(false), true);

  int i = 4;
  VERIFY_ASSERT(foo(i), true);
  VERIFY_ASSERT(contract_assert( i == 4 ? true : false), false)
  VERIFY_ASSERT(contract_assert( i > 4 ? true : false), true)

  i = 18;
  VERIFY_ASSERT(foo(i), false);
  VERIFY_ASSERT(contract_assert( i == 4 ? true : false), true)
  VERIFY_ASSERT(contract_assert( i > 4 ? true : false), false)

  return 0;
}
