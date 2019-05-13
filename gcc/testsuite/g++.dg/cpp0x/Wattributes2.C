// PR c++/60373
// { dg-do compile { target c++11 } }
// { dg-require-visibility "" }

#pragma GCC visibility push(default)
namespace std
{
  using size_t = decltype(sizeof(0));
  struct nothrow_t { };
}

void* operator new(std::size_t)
  __attribute__((__externally_visible__));
void* operator new[](std::size_t)
  __attribute__((__externally_visible__));
void* operator new(std::size_t, const std::nothrow_t&) noexcept
  __attribute__((__externally_visible__, __malloc__));
void* operator new[](std::size_t, const std::nothrow_t&) noexcept
  __attribute__((__externally_visible__, __malloc__));
void operator delete(void*) noexcept
  __attribute__((__externally_visible__));
void operator delete[](void*) noexcept
  __attribute__((__externally_visible__));
void operator delete(void*, const std::nothrow_t&) noexcept
  __attribute__((__externally_visible__));
void operator delete[](void*, const std::nothrow_t&) noexcept
  __attribute__((__externally_visible__));
#pragma GCC visibility pop

__attribute__((visibility("hidden")))void*operator new(std::size_t); // { dg-warning "visibility attribute ignored" }

// { dg-message "previous declaration" "" { target *-*-* } 12 }
