// PR c++/90736 - bogus error with alignof.
// { dg-do compile { target c++11 } }

constexpr int fn(const int b) { return b; }
constexpr int c = fn(alignof(int));
alignas(c) char d;
