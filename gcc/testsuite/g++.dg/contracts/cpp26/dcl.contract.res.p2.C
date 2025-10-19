// N5008 :
// dcl.contract.res/p2
// When the declared return type of a non-templated function contains a placeholder type, a postcondition-
// specifier with a result-name-introducer shall be present only on a definition.
// { dg-do compile { target c++23 } }
// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=observe " }


auto no_deduced_res_types_on_non_defs (int x) // { dg-error "postconditions with deduced result name types must only appear on function definitions" }
  pre (x > 1)
  post (r: r > 17); 

// =====

auto g(auto&)
post (r: r >= 0); // OK, g is a template.

auto f2() post (r : r > 0) // OK, type of r is deduced below.
{ return 5; }

template <typename T>
auto f3 () post (r: r > 0); // OK, postcondition instantiated with template

auto f4 () post (true); // OK, return value not named

