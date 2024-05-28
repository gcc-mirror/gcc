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

// { dg-prune-output "not declared" }

static_assert (__cpp_contracts >= 201906);
static_assert (__cpp_contracts_literal_semantics >= 201906);
static_assert (__cpp_contracts_roles >= 201906);


contract_assert f(); // { dg-error "expected unqualified-id before" }
void f(contract_assert); // { dg-error "expected primary-expression before"}
struct contract_assert{}; // { dg-error "expected unqualified-id before" }
void contract_assert();
int main()
{

	contract_assert(x==0); // { dg-error }
	contract_assert int i = 0; // { dg-error }

	i = 7;
	[[assert: i == 0]] contract_assert(x==0); // { dg-error }

	contract_assert( x = 0); // { dg-error  "expected .). before .=. token"}

    contract_assert( y == 0); // { dg-error ".y. was not declared in this scope" }
	return 0;
}
