// { dg-do compile { target c++20 } }

struct S {
  constexpr S () : s (0) {}
  virtual int foo () const { return 42; }
  consteval virtual int bar () const { return 43; }
  consteval virtual int baz () const { return 44; }
  int s;
};
struct T : public S {
  constexpr T () : t (0) {}
  consteval int bar () const { return 45; }
  consteval virtual int baz () const { return 46; }
  int t;
};

consteval int
foo ()
{
  S s;
  T t;
  S *u = (S *) &t;
  T *v = &t;
  auto pmf1 = &S::bar;
  auto pmf2 = &S::baz;
  if ((s.*pmf1) () != 43) throw 1;
  if ((s.*pmf2) () != 44) throw 2;
  if ((t.*pmf1) () != 45) throw 3;
  if ((t.*pmf2) () != 46) throw 4;
  if ((u->*pmf1) () != 45) throw 5;
  if ((u->*pmf2) () != 46) throw 6;
  return 0;
}

constexpr S s;
constexpr T t;

constexpr const S *
bar (bool x)
{
  return x ? &s : (const S *) &t;
}

int a = foo ();
int b = bar (false)->bar ();
int c = bar (true)->baz ();
static_assert (bar (false)->bar () == 45);
static_assert (bar (true)->baz () == 44);
