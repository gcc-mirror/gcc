// PR c++/99701
// { dg-do compile { target c++20 } }

#include <compare>

decltype(nullptr) foo ();

auto b0 = nullptr <=> nullptr; // { dg-error "ordered comparison" }
auto b1 = 0 <=> nullptr; // { dg-error "ordered comparison" }
auto b2 = nullptr <=> 0; // { dg-error "ordered comparison" }
auto b3 = foo () <=> 0; // { dg-error "ordered comparison" }
auto b4 = 0 <=> foo (); // { dg-error "ordered comparison" }
auto b5 = foo () <=> nullptr; // { dg-error "ordered comparison" }
auto b6 = nullptr <=> foo (); // { dg-error "ordered comparison" }
