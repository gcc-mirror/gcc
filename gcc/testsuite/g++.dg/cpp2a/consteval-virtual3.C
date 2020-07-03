// { dg-do compile { target c++20 } }

struct S {
  constexpr S () : s (0) {}
  virtual int foo () const { return 42; }
  consteval virtual int bar () const { return 43; }
  consteval virtual int baz () const { return 44; }
  consteval virtual int qux () const { return 47; }
  int s;
};
struct T : public S {
  constexpr T () : t (0) {}
  consteval int bar () const { return 45; }
  consteval virtual int baz () const { return 46; }
  consteval virtual int grault () const { return 48; }
  int t;
};

consteval int
foo ()
{
  S s;
  T t;
  S *u = (S *) &t;
  T *v = &t;
  if (s.bar () != 43) throw 1;
  if (s.baz () != 44) throw 2;
  if (t.bar () != 45) throw 3;
  if (t.baz () != 46) throw 4;
  if (u->bar () != 45) throw 5;
  if (u->baz () != 46) throw 6;
  if (s.qux () != 47) throw 7;
  if (t.qux () != 47) throw 8;
  if (u->qux () != 47) throw 9;
  if (v->qux () != 47) throw 10;
  if (v->grault () != 48) throw 11;
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
