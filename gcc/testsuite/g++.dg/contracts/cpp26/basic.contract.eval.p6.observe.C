// N5008 :
// basic.contract.eval/p6
// There is an observable checkpoint ([intro.abstract]) C that happens before
// (evaluation of a contract assertion ) A such that any other operation O
// that happens before A also happens before C
// { dg-do compile { target c++26 } }
// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=observe -fdump-tree-gimple " }


int main(int ac, char *av[])
{
   int i = ac;
   contract_assert (i == 3);
  return i;
}

// { dg-final { scan-tree-dump "__builtin_observable.*__tu_has_violation.*__builtin_observable" "gimple" } }
