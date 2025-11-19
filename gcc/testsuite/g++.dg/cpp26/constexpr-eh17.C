// PR libstdc++/122671
// { dg-do compile { target c++26 } }

#include <new>
#include <memory>

consteval auto
foo ()
{
  struct E {};
  struct O
  {
    constexpr explicit O (int x)
    {
      if (x < 0) { throw E {}; }
    }
  };

  try
  {
    struct S
    {
      O *s;
      constexpr S () : s { std::allocator <O> {}.allocate (1) } {}
      constexpr ~S () { std::allocator <O> {}.deallocate (s, 1); }
    };

    auto s = S {};

    ::new (s.s) O { -1 };
  }
  catch (E &)
  {
  }
  return true;
}

static_assert (foo ());
