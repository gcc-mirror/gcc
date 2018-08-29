// PR c++/85713
// { dg-do compile { target c++14 } }

auto l3 = [](auto v, auto (&array) [5]) -> int { return v + array[0]; };
