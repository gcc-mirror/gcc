// PR c++/124012
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

void foo (char);
void corge (const char *);
struct A { char a; decltype (^^::) b; };

void
bar ()
{
  constexpr auto [a, b] = A {};
  foo (a);
}

void
baz ()
{
  constexpr auto a = A {};
  foo (a.a);
}

void
qux ()
{
  constexpr auto a = A {};
  corge (&a.a);
}

void
garply ()
{
  constexpr auto [a, b] = A {};
  corge (&a);
}

void
fred ()
{
  constexpr auto [a, b] = A {};
  constexpr auto c = a;
  foo (c);
  corge (&c);
}
