// This started to be miscompiled with r15-579 or in a larger
// test with r15-3956 and got fixed with r17-2039.
// DSE would optimize away the store of 42.
// { dg-do run { target c++11 } }
// { dg-options "-O2" }

struct B { };

void *volatile g;

template <typename T>
struct D { T foo (void) const { return (T) g; } };

struct G : public B
{
  virtual B *bar (int);
  struct H : public B { H () : h (42) {} int h; };
  struct I : public B {};
  D <I *> g;
};

struct H : public G { virtual B *bar (int) { return nullptr; } };

B *
G::bar (int x)
{
  if (x == 0)
    return new H ();
  I *y = g.foo ();
  return y;
}

[[gnu::noipa]] B *
baz (G *x, int y)
{
  return x->bar (y);
}

int
main ()
{
  G g;
  G::H *h = (G::H *) baz (&g, 0);
  if (h->h != 42)
    __builtin_abort ();
}
