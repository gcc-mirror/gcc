// PR middle-end/116896
// { dg-do run { target c++20 } }
// { dg-options "-O2" }

#include "pr116896-1.C"

[[gnu::noipa]] auto
corge (int x)
{
  return x <=> 0;
}

[[gnu::noipa]] auto
garply (unsigned x)
{
  return x <=> 0;
}

int
main ()
{
  if (foo (-1.0f, 1.0f) != std::partial_ordering::less
      || foo (1.0f, -1.0f) != std::partial_ordering::greater
      || foo (1.0f, 1.0f) != std::partial_ordering::equivalent
      || foo (__builtin_nanf (""), 1.0f) != std::partial_ordering::unordered
      || bar (-2.0f, 2.0f) != std::partial_ordering::less
      || bar (2.0f, -2.0f) != std::partial_ordering::greater
      || bar (-5.0f, -5.0f) != std::partial_ordering::equivalent
      || baz (-42, 42) != std::strong_ordering::less
      || baz (42, -42) != std::strong_ordering::greater
      || baz (42, 42) != std::strong_ordering::equal
      || qux (40, 42) != std::strong_ordering::less
      || qux (42, 40) != std::strong_ordering::greater
      || qux (40, 40) != std::strong_ordering::equal
      || corge (-15) != std::strong_ordering::less
      || corge (15) != std::strong_ordering::greater
      || corge (0) != std::strong_ordering::equal
      || garply (15) != std::strong_ordering::greater
      || garply (0) != std::strong_ordering::equal)
    __builtin_abort ();
}
