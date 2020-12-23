// { dg-additional-options "-fmodules-ts -std=c++2a" }
export module foo;
// { dg-module-cmi foo }

struct tuple { int a, b, c;};

tuple maker ();

export inline int bob ()
{
  auto [a, b, c] = maker ();

  return a + b + c;
}
