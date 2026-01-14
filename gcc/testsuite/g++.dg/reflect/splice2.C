// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

constexpr int foo (int) { return 42; };
constexpr auto bar = ^^foo;
constexpr int foo (long) { return 43; }
constexpr int foo (long long) { return 44; }
static_assert (foo (0) == 42);
static_assert (foo (0L) == 43);
static_assert (foo (0LL) == 44);
static_assert ([: bar :] (0) == 42);
static_assert ([: bar :] (0L) == 42);
static_assert ([: bar :] (0LL) == 42);
