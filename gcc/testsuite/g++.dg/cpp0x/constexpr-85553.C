// PR c++/85553
// { dg-do compile { target c++11 } }
using T = decltype(nullptr);
const constexpr T foo{};
