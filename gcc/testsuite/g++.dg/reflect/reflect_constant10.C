// PR c++/125649
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <memory>
#include <meta>

consteval {
  const int *m = std::allocator<int>().allocate(1);
  std::meta::reflect_constant(m);
} // { dg-error "reflect_constant failed" }

consteval {
  const int *m = std::allocator<int>().allocate(1);
  std::meta::reflect_constant(m + 1);
} // { dg-error "reflect_constant failed" }

// This is well-formed.
consteval {
  int *p = std::allocator<int>().allocate(1);
  std::allocator<int>().deallocate(p, 1);
  bool threw = false;
  try { std::meta::reflect_constant(p); }
  catch (std::meta::exception&) { threw = true; }
  if (!threw)
    throw;
}

// This is well-formed.
consteval {
  int *p = std::allocator<int>().allocate(1);
  bool threw = false;
  try { std::meta::reflect_constant(p); }
  catch (std::meta::exception&) { threw = true; }
  std::allocator<int>().deallocate(p, 1);
  if (!threw)
    throw;
}
