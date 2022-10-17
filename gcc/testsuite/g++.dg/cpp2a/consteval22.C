// PR c++/102753
// { dg-do compile { target c++20 } }

struct S {
  constexpr S () : s (0) {}
  consteval int foo () { return 1; }
  virtual consteval int bar () { return 2; }
  int s;
};
typedef int (S::*P) ();

consteval P
foo ()
{
  return &S::foo;
}

consteval P
bar ()
{
  return &S::bar;
}

consteval int
baz ()
{
  S s;
  return (s.*(foo ())) () + (s.*(bar ())) ();
}

static_assert (baz () == 3);

constexpr P a = foo ();		// { dg-error "immediate evaluation returns address of immediate function" }
constexpr P b = bar ();		// { dg-error "immediate evaluation returns address of immediate function" }
