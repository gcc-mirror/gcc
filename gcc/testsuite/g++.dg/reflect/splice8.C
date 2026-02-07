// PR c++/123752
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct S { int s; };
struct U { int u; };

template <typename T>
consteval decltype (^^int)
foo ()
{
  return ^^S::s;
}

template <>
consteval decltype (^^int)
foo <U> ()
{
  throw 1;
}

template <typename T>
auto
bar (T x)
{
  return x.[: foo <T> () :];	// { dg-error "uncaught exception '1'" }
}

auto
baz (U x)
{
  return bar (x);
}
