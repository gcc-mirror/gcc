// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test reflections on classes.

struct S {
  using T = int;
};

void
f1 ()
{
  constexpr auto s = ^^S;
  [: s :]::T i = 42;
  typename [: s :]::T j = 42;
}
