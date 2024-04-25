// PR c++/111284
// { dg-do compile { target c++17 } }

struct S {
  S () = default;
  constexpr S (const S &) noexcept : s{this} {}
  constexpr S & operator= (const S &) noexcept { return *this; }
  constexpr bool foo () const noexcept { return s == this; }
  S *s = this;
};

constexpr bool
bar (S x) noexcept
{
  return x.foo ();
}

static_assert (bar (S {}), "");
static_assert ([] (S x) { return x.foo (); } (S {}), "");
