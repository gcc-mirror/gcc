// { dg-do compile { target c++11 } }

constexpr int foo() { thread_local int i __attribute__((unused)) {}; return 1; }  // { dg-error "40:.i. defined .thread_local." "" { target c++20_down } }

constexpr int bar() { static int i __attribute__((unused)) {}; return 1; }  // { dg-error "34:.i. defined .static." "" { target c++20_down } }
