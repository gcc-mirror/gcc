// { dg-options "-fno-contracts -std=c++26" }

void f()
{
    contract_assert(false); // { dg-error "'contract_assert' is only available with '-fcontracts'" }
    __contract_assert(false); // { dg-error "'__contract_assert' is only available with '-fcontracts'" }
}
