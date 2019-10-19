// PR c++/83820 - excessive attribute arguments not detected.
// { dg-do compile { target c++11 } }

[[noreturn()]] void f0 (); // { dg-error ".noreturn. attribute does not take any arguments" }
[[noreturn(1)]] void f1 (); // { dg-error ".noreturn. attribute does not take any arguments" }
[[noreturn(1, 2)]] void f2 (); // { dg-error ".noreturn. attribute does not take any arguments" }
[[maybe_unused()]] int f3(); // { dg-error ".maybe_unused. attribute does not take any arguments" }
[[nodiscard()]] int f4(); // { dg-error "parentheses must be omitted if .nodiscard. attribute argument list is empty" }
[[gnu::noinline()]] int f5(); // { dg-error ".noinline. attribute does not take any arguments" }
[[gnu::constructor]] int f6();
[[gnu::constructor(101)]] int f7(); // { dg-error "constructor priorities are not supported" "" { target { ! init_priority } } }
