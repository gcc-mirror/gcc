// PR c++/98122
// { dg-do compile { target c++14 } }

union U { int a; };

constexpr bool
foo ()
{
  U f { 42 };
  constexpr auto m = &U::a;
  return (f.*m) == 42;
}

static_assert (foo (), "");
