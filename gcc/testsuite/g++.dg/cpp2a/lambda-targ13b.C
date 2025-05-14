// PR c++/119574
// A version of lambda-targ13.C where the inner lambda returns non-void.
// { dg-do compile { target c++20 } }

template <class F = decltype([] <auto G = [] { return 42; }> () { return G(); })>
constexpr int f(F op = {}) { return op(); }

static_assert(f() == 42);
