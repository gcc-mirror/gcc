// { dg-do compile }
// { dg-options "-O2 -fdump-tree-evrp-details" }

static int isNaN(double x)
{
    return x != x;
}

static double opCmpProper(int lhs, double rhs)
{
  return lhs < rhs ? -1.0
       : lhs > rhs ? 1.0
       : lhs == rhs ? 0.0
       : __builtin_nan("");
}

int main()
{
    if (!isNaN(opCmpProper(41, __builtin_nan(""))))
      __builtin_abort();
    return 0;
}

// { dg-final {scan-tree-dump-not "Folds to: 0.0" "evrp" } }
