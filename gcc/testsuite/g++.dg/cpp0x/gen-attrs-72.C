// DR 1914 - Duplicate standard attributes 
// { dg-do compile { target c++11 } }

#define ATTR_NORETURN [[noreturn, noreturn]] 

[[noreturn, noreturn]] void fn0(); // { dg-warning "specified multiple times" }
ATTR_NORETURN void fn0a();
[[noreturn]] [[noreturn]] void fn1();
[[deprecated, deprecated]] void fn2(); // { dg-warning "specified multiple times" }
[[deprecated]] [[deprecated]] void fn3();
[[maybe_unused]] [[maybe_unused]] int fn4();
[[maybe_unused, maybe_unused]] int fn5(); // { dg-warning "specified multiple times" }
[[nodiscard]] [[nodiscard]] int fn6();
[[nodiscard, nodiscard]] int fn7(); // { dg-warning "specified multiple times" }

struct E { };
struct A {
  [[no_unique_address]] [[no_unique_address]] E e;
};
struct B {
  [[no_unique_address, no_unique_address]] E e; // { dg-warning "specified multiple times" }
};

int
f (int n)
{
  switch (n)
    {
    case 1:
      [[fallthrough, fallthrough]]; // { dg-warning "specified multiple times" }
    case 2:
      [[fallthrough]] [[fallthrough]]; // { dg-warning "specified multiple times" }
    case 3:
      return 15;
    }

  if (n == 10)
    [[likely]] [[likely]] return 42; // { dg-warning "ignoring attribute" }
  else if (n == 11)
    [[unlikely]] [[unlikely]] return 10; // { dg-warning "ignoring attribute" }
  else if (n == 12)
    [[likely, likely]] return 42; // { dg-warning "specified multiple times" }
  else
    [[unlikely, unlikely]] return 0; // { dg-warning "specified multiple times" }
}
