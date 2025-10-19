// generic assertion-statement parsing checks
// N5008
// assertion-statement :
// contract_assert attribute-specifier-seq opt ( conditional-expression ) ;
// { dg-do compile { target c++26 } }
// { dg-additional-options "-fcontracts" }

// { dg-prune-output "not declared" }

static_assert (__cpp_contracts >= 202502L);

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

    contract_assert(true) // { dg-error  "expected .;. before .int." }
    int k = 4;

    contract_assert(true) // { dg-error  "expected .;. before .g." }
    g();

    contract_assert [[deprecated]] (i == 3);  // { dg-warning {attributes are ignored on 'contract_assert'} }

    return 0;
}
