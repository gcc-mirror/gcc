// PR c++/123659
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

struct S {};

template <int...I>
void
foo ()
{
  static constexpr std::meta::info t[] { ^^int, ^^long, ^^S, ^^double };
  [] (typename [: t[I] :]... a) {} (1, S {}, 2.5, 3L, 4L);
}

void
bar ()
{
  foo <0, 2, 3, 1, 1> ();
}
