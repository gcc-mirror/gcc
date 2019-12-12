// { dg-do compile { target c++11 } }

constexpr int foo() { thread_local int i __attribute__((unused)) {}; return 1; }  // { dg-error "40:.i. declared .thread_local." }

constexpr int bar() { static int i __attribute__((unused)) {}; return 1; }  // { dg-error "34:.i. declared .static." }
