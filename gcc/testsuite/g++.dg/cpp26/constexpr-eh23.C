// PR c++/126508
// { dg-do compile { target c++26 } }

#include <exception>

constexpr std::exception_ptr
foo (const std::exception_ptr &x)
{
  return x;
}

constexpr bool
baz ()
{
  std::exception_ptr a = std::make_exception_ptr (42);
  auto b = foo (a);
  auto c = foo (a);
  auto d = foo (a);
  return true;
}

static_assert (baz ());
