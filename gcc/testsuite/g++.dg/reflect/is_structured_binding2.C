// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

struct S
{
  int a, b;
  explicit operator bool () const noexcept { return a == b; }
};

template <typename S>
void
bar ()
{
  if (auto [i, j] = S{ 7, 7 })
    {
      constexpr auto ri = ^^i;
      constexpr auto rj = ^^j;

      static_assert (std::meta::is_structured_binding (ri));
      static_assert (std::meta::is_structured_binding (rj));

      static_assert (std::meta::is_structured_binding (^^i));
      static_assert (std::meta::is_structured_binding (^^j));

      static_assert (!std::meta::is_structured_binding (^^S));
    }
}

int
main ()
{
  bar<S> ();
}
