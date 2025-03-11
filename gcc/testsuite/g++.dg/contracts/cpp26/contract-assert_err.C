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


contract_assert f(); // { dg-error "expected unqualified-id before .contract_assert." }

void f(contract_assert); // { dg-error "expected primary-expression before|declared void" }
struct contract_assert{}; // { dg-error "expected unqualified-id before|expected identifier" }
void contract_assert(); // { dg-error "expected unqualified-id before" }

void g(){};
int main()
{
    contract_assert(x==0); // { dg-error ".x. was not declared in this scope"}
    contract_assert int i = 0; // // { dg-error "expected primary-expression before|before .int." }

    i = 7;

    int j = 4;

    contract_assert( x = 0); // { dg-error  "expected primary-expression before|expected .\\). before .=. token" }

    contract_assert( y == 0); // { dg-error ".y. was not declared in this scope" }

    contract_assert(true)
    int k = 4; // { dg-error  "expected semicolon before .int." }

    contract_assert(true) // { dg-error  "contract assertion on a non empty statement" }
    g(); // { dg-error  "expected semicolon before .g." }

    return 0;
}
