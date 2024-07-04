// PR c++/115754
// { dg-do compile { target c++26 } }

namespace std
{
  using size_t = decltype (sizeof 0);

  template <typename T>
  struct allocator
  {
    constexpr allocator () noexcept {}

    constexpr T *allocate (size_t n)
    { return static_cast<T *> (::operator new (n * sizeof(T))); }

    constexpr void
    deallocate (T *p, size_t n)
    { ::operator delete (p); }
  };
}

constexpr void *
operator new (std::size_t, void *p) noexcept
{ return p; }

constexpr bool
foo ()
{
  std::allocator<int> a;
  auto b = a.allocate (1);
  ::new (b) int ();
  a.deallocate (b, 1);
  return true;
}

constexpr bool a = foo ();
