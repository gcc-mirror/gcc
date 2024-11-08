//
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr -fcontract-evaluation-semantic=observe -fdump-tree-gimple " }


int main(int ac, char *av[])
{
   int i = ac;
   contract_assert (i == 3);
  return i;
}

// { dg-final { scan-tree-dump "__builtin_observable.*handle_contract_violation.*__builtin_observable" "gimple" } }
