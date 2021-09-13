// PR c++/90333
// { dg-do compile { target c++11 } }

auto x = []() __attribute__((always_inline)) -> int { return 0; };
