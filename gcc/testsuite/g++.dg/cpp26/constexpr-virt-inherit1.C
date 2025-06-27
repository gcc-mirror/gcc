// C++26 P3533R2 - constexpr virtual inheritance
// { dg-do compile { target c++26 } }

struct A {
  int a;
  constexpr virtual int foo () { return a; };
  constexpr A () : a (42) {}
  constexpr A (int x) : a (x) {}
  constexpr virtual ~A () { if (a < 42 || a > 62) asm (""); }
};
struct B : public A {
  int b;
  constexpr virtual int foo () { return a + b; }
  constexpr B () : A (43), b (42) {}
  constexpr B (int x, int y) : A (x), b (y) {}
  constexpr virtual ~B () { if (b < 42 || b > 62) asm (""); }
};
struct C : virtual public B {
  int c;
  constexpr C () : B (44, 43), c (45) {}
  constexpr C (int x) : B (44, 43), c (x) {}
  constexpr virtual int bar () { return a + b + c; }
  constexpr virtual ~C () { if (c < 42 || c > 62) asm (""); }
};
struct D : virtual public B {
  int d;
  constexpr D () : B (44, 43), d (45) {}
  constexpr D (int x) : B (44, 43), d (x) {}
  constexpr virtual int baz () { return a + b + d; }
  constexpr virtual ~D () { if (d < 42 || d > 62) asm (""); }
};
struct E : public C, D {
  int e;
  constexpr E () : B (), C (), D (), e (58) {}
  constexpr E (int x, int y, int z, int w, int v) : B (x, y), C (z), D (w), e (v) {}
  constexpr virtual ~E () { if (e < 42 || e > 62) asm (""); }
};

constexpr bool
qux ()
{
  E f (45, 46, 47, 48, 49);
  f.a++;
  f.b++;
  f.c++;
  f.d++;
  f.e++;
  C *c = static_cast <C *> (&f);
  D *d = static_cast <D *> (&f);
  B *b = static_cast <B *> (&f);
  A *a = static_cast <A *> (&f);
  if (f.foo () != 46 + 47)
    return false;
  if (f.bar () != 46 + 47 + 48)
    return false;
  if (f.baz () != 46 + 47 + 49)
    return false;
  a->a += 2;
  b->b += 3;
  c->c += 4;
  c->a += 5;
  d->d += 6;
  d->a += 7;
  if (c->foo () != 60 + 50)
    return false;
  c->b -= 3;
  if (d->foo () != 60 + 47)
    return false;
  if (f.a != 60 || f.b != 47 || f.c != 52 || f.d != 55 || f.e != 50)
    return false;
  C g (48);
  c = static_cast <C *> (&g);
  b = static_cast <B *> (&g);
  a = static_cast <A *> (&g);
  g.a++;
  g.b++;
  g.c++;
  if (g.foo () != 45 + 44)
    return false;
  if (g.bar () != 45 + 44 + 49)
    return false;
  a->a += 2;
  b->b += 3;
  c->c += 4;
  if (c->foo () != 47 + 47)
    return false;
  if (g.a != 47 || g.b != 47 || g.c != 53)
    return false;
  D h (49);
  d = static_cast <D *> (&h);
  b = static_cast <B *> (&h);
  a = static_cast <A *> (&h);
  h.a++;
  h.b++;
  h.d++;
  if (h.foo () != 45 + 44)
    return false;
  if (h.baz () != 45 + 44 + 50)
    return false;
  a->a += 2;
  b->b += 3;
  d->d += 4;
  if (d->foo () != 47 + 47)
    return false;
  if (h.a != 47 || h.b != 47 || h.d != 54)
    return false;
  return true;
}

constexpr bool
corge ()
{
  E *f = new E (45, 46, 47, 48, 49);
  f->a++;
  f->b++;
  f->c++;
  f->d++;
  f->e++;
  C *c = static_cast <C *> (f);
  D *d = static_cast <D *> (f);
  B *b = static_cast <B *> (f);
  A *a = static_cast <A *> (f);
  if (f->foo () != 46 + 47)
    return false;
  if (f->bar () != 46 + 47 + 48)
    return false;
  if (f->baz () != 46 + 47 + 49)
    return false;
  a->a += 2;
  b->b += 3;
  c->c += 4;
  c->a += 5;
  d->d += 6;
  d->a += 7;
  if (c->foo () != 60 + 50)
    return false;
  c->b -= 3;
  if (d->foo () != 60 + 47)
    return false;
  if (f->a != 60 || f->b != 47 || f->c != 52 || f->d != 55 || f->e != 50)
    return false;
  C *g = new C (48);
  c = static_cast <C *> (g);
  b = static_cast <B *> (g);
  a = static_cast <A *> (g);
  g->a++;
  g->b++;
  g->c++;
  if (g->foo () != 45 + 44)
    return false;
  if (g->bar () != 45 + 44 + 49)
    return false;
  a->a += 2;
  b->b += 3;
  c->c += 4;
  if (c->foo () != 47 + 47)
    return false;
  if (g->a != 47 || g->b != 47 || g->c != 53)
    return false;
  D *h = new D (49);
  d = static_cast <D *> (h);
  b = static_cast <B *> (h);
  a = static_cast <A *> (h);
  h->a++;
  h->b++;
  h->d++;
  if (h->foo () != 45 + 44)
    return false;
  if (h->baz () != 45 + 44 + 50)
    return false;
  a->a += 2;
  b->b += 3;
  d->d += 4;
  if (d->foo () != 47 + 47)
    return false;
  if (h->a != 47 || h->b != 47 || h->d != 54)
    return false;
  delete h;
  delete g;
  delete f;
  return true;
}

static_assert (qux ());
static_assert (corge ());
constexpr E a;
constexpr E b (45, 46, 47, 48, 49);
constexpr C c;
constexpr C d (50);
