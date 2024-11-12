//
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr -O -fdump-tree-original -fdump-tree-optimized -Wno-return-type" }
int f(int& i)
{
   i++;
   return i;
}

int f2 (int x, const int *y)
{
    int b = x + *y;
}

int main(int ac, char *av[])
{
   int i = 3;
   *av[0] = (char) f(i);
   contract_assert (f2(i, &i) > 42);
  return i;
}
// Check that
// - observable checkpoint has been added to the contract_assert
// - observable has been removed from the optimized tree

// { dg-final { scan-tree-dump "__builtin_observable" "original" } }
// { dg-final { scan-tree-dump "_2 = 4" "optimized" } }
