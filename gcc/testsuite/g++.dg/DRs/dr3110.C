// DR 3110, Constexpr allocation for literal types
// { dg-do compile { target c++20 } }

#include <locale>
#include <memory>

static_assert([]{
  auto a = std::allocator<std::locale>{};
  a.deallocate(a.allocate(42), 42);
  return true;
}());
