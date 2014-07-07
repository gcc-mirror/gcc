// PR c++/51400
// { dg-do compile { target c++11 } }

constexpr int (*f)() __attribute__((noreturn)) = 0;
constexpr int (*g)() __attribute__((const)) = 0;
