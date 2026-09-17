// Check that invoking a function during constant evaluation, then calling the
// same function with different arguments within a contract assertion, works.
// { dg-do compile { target c++26 } }
// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=enforce" }

constexpr bool f(int) { return true; }

constexpr bool g()
{
    f(0);
    contract_assert(f(1));  // force another evaluation of f
    return true;
}

static_assert(g());
