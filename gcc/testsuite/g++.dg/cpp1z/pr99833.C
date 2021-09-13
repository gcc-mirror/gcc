// PR c++/99833
// { dg-do compile { target c++17 } }

struct S { int a, b; };
template <class>
void
foo ()
{
  [](auto d) { if constexpr (auto [a, b]{d}; sizeof (a) > 0) a++; } (S{});
}
template void foo<S> ();
