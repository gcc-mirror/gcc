// PR c++/98122
// { dg-do compile { target c++20 } }

union V { int a; char b; };
union W { int a; int b; };

constexpr bool
bar ()
{
  V f { .b = 42 };
  constexpr auto m = &V::a;
  return (f.*m) == 42;
}

constexpr bool
baz ()
{
  W f { .b = 42 };
  constexpr auto m = &W::b;
  return (f.*m) == 42;
}

static_assert (bar (), "");	// { dg-error "non-constant condition for static assertion" }
				// { dg-error "accessing 'V::a' member instead of initialized 'V::b' member in constant expression" "" { target *-*-* } .-1 }
static_assert (baz (), "");
