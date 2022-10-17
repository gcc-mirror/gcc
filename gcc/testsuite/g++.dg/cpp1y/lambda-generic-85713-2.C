// PR c++/85713
// { dg-do compile { target c++14 } }

auto l4 = [](auto v, auto (&array (int)) [5]) -> int { return v + array[0]; };
auto l5 = [](auto v, auto (&array (auto)) [5]) -> int { return v + array[0]; };	    // { dg-error ".auto. parameter not permitted in this context" }
auto l6 = [](auto v, auto (&array (int int)) [5]) -> int { return v + array[0]; };  // { dg-error "two or more data types" }
auto l7 = [](auto v, auto (&array (int auto)) [5]) -> int { return v + array[0]; };  // { dg-error "two or more data types" }
