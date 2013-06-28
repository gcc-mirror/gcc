// PR c++/52216
// { dg-require-effective-target c++11 }

#include <new>

int n;

static_assert(!noexcept(::new (std::nothrow) int[n]), "");
