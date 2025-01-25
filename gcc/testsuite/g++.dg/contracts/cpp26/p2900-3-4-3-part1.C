// { dg-options "-std=c++26 -fcontracts -fcontracts-nonattr" }

int bad_mr_shadow (int r)
  post (r: r > 5) // { dg-error "contract postcondition result names must not shadow function parameters" }
  { return r + 1; }

auto no_deduced_res_types_on_non_defs (int x) // { dg-error "postconditions with deduced result name types must only appear on function definitions" }
  pre (x > 1)
  post (r: r > 17); 

// =====

auto f2() post (r : r > 0) // OK, type of r is deduced below.
{ return 5; }

template <typename T>
auto f3 () post (r: r > 0); // OK, postcondition instantiated with template

auto f4 () post (true); // OK, return value not named