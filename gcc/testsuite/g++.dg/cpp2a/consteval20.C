// PR c++/102753
// { dg-do compile { target c++20 } }

struct S {
  consteval int foo () const { return 42; }
};

constexpr S s;

int
bar ()
{
  auto c = &S::foo;			// { dg-error "taking address of an immediate function" }
  constexpr auto d = &S::foo;		// { dg-error "constant evaluation returns address of immediate function" }
  static auto e = &S::foo;		// { dg-error "taking address of an immediate function" }
  return (s.*&S::foo) ();		// { dg-error "taking address of an immediate function" }
}

constexpr auto a = &S::foo;		// { dg-error "constant evaluation returns address of immediate function" }
auto b = &S::foo;			// { dg-error "taking address of an immediate function" }

consteval int
baz ()
{
  return (s.*&S::foo) ();
}

static_assert (baz () == 42);
