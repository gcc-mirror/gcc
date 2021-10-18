// { dg-do compile { target c++11 } }

constexpr int foo() { thread_local int i __attribute__((unused)) {}; return 1; }  // { dg-error "40:.i. declared .thread_local." "" { target c++20_down } }
// { dg-error "40:.i. declared .thread_local. in .constexpr. context" "" { target c++23 } .-1 }

constexpr int bar() { static int i __attribute__((unused)) {}; return 1; }  // { dg-error "34:.i. declared .static." "" { target c++20_down } }
// { dg-error "34:.i. declared .static. in .constexpr. context" "" { target c++23 } .-1 }
