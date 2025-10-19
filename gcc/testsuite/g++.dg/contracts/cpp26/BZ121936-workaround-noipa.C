// check that we do not optimise based on contract_asssert
// { dg-additional-options "-O3 -fcontracts -fcontract-evaluation-semantic=quick_enforce -fdump-tree-optimized " }
// { dg-do compile { target c++26 } }

[[gnu::used, gnu::noinline]]
inline int* f(int *y) {
  contract_assert(y != nullptr);
  return y;
}


int foo(int *x) {
    if (f(x) == nullptr)
    {
        return 0;
    }
    return *x;
}

// Check that the if condition in foo isn't optimised away, we should see the
// nullptr check twice.
// { dg-final { scan-tree-dump-times {if \([^=]+== 0B\)} 2 "optimized" } }
