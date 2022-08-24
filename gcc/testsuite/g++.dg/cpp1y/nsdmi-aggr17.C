// PR c++/100252
// { dg-do run { target c++14 } }

#define SA(X) static_assert ((X),#X)

struct A {
  int x;
  int y = x;
  const A* p = this;
};

struct B {
  int x = 42;
  A a = A{x};
};

constexpr B b;
SA(b.a.p == &b.a);
SA(b.x == 42);
B b2 = { };
B b3 = { 42 };

struct C {
  int x = 42;
  B b = B{x};
};

constexpr C c;
C c2;
C c3;

struct D {
  int x = 42;
  A a = (true, A{x});
};

constexpr D d;
SA(d.a.p == &d.a);
SA(d.x == 42);
D d2 = { };
D d3 = { 42 };

struct E {
  int x = 42;
  A a = (A{x});
};

constexpr E e;
SA(e.a.p == &e.a);
SA(e.x == 42);
E e2 = { };
E e3 = { 42 };

struct F {
  int x = 42;
  A a = true ? A{x} : A{x};
};

constexpr F f;
SA (f.a.p == &f.a);
SA (e.x == 42);
F f2 = { };
F f3 = { 42 };

static void
test_b (B b4 = B{}, B b5 = B{ 42 })
{
  if (b2.x != 42 || b2.a.x != 42 || b2.a.y != b2.a.x)
    __builtin_abort ();
  if (b3.x != 42 || b3.a.x != 42 || b3.a.y != b3.a.x)
    __builtin_abort ();
  if (b4.x != 42 || b4.a.x != 42 || b4.a.y != b4.a.x)
    __builtin_abort ();
  if (b5.x != 42 || b5.a.x != 42 || b5.a.y != b5.a.x)
    __builtin_abort ();
}

static void
test_c (C c4 = C{}, C c5 = C{ 42 })
{
  if (c2.b.x != 42 || c2.b.a.x != 42 || c2.b.a.y != c2.b.a.x)
    __builtin_abort ();
  if (c3.b.x != 42 || c3.b.a.x != 42 || c3.b.a.y != c3.b.a.x)
    __builtin_abort ();
  if (c4.b.x != 42 || c4.b.a.x != 42 || c4.b.a.y != c4.b.a.x)
    __builtin_abort ();
  if (c5.b.x != 42 || c5.b.a.x != 42 || c5.b.a.y != c5.b.a.x)
    __builtin_abort ();
}

static void
test_d (D d4 = D{}, D d5 = D{ 42 })
{
  if (d2.x != 42 || d2.a.x != 42 || d2.a.y != d2.a.x)
    __builtin_abort ();
  if (d3.x != 42 || d3.a.x != 42 || d3.a.y != d3.a.x)
    __builtin_abort ();
  if (d4.x != 42 || d4.a.x != 42 || d4.a.y != d4.a.x)
    __builtin_abort ();
  if (d5.x != 42 || d5.a.x != 42 || d5.a.y != d5.a.x)
    __builtin_abort ();
}

static void
test_e (E e4 = E{}, E e5 = E{ 42 })
{
  if (e2.x != 42 || e2.a.x != 42 || e2.a.y != e2.a.x)
    __builtin_abort ();
  if (e3.x != 42 || e3.a.x != 42 || e3.a.y != e3.a.x)
    __builtin_abort ();
  if (e4.x != 42 || e4.a.x != 42 || e4.a.y != e4.a.x)
    __builtin_abort ();
  if (e5.x != 42 || e5.a.x != 42 || e5.a.y != e5.a.x)
    __builtin_abort ();
}

static void
test_f (F f4 = F{}, F f5 = F{ 42 })
{
  if (f2.x != 42 || f2.a.x != 42 || f2.a.y != f2.a.x)
    __builtin_abort ();
  if (f3.x != 42 || f3.a.x != 42 || f3.a.y != f3.a.x)
    __builtin_abort ();
  if (f4.x != 42 || f4.a.x != 42 || f4.a.y != f4.a.x)
    __builtin_abort ();
  if (f5.x != 42 || f5.a.x != 42 || f5.a.y != f5.a.x)
    __builtin_abort ();
}
int
main ()
{
  test_b ();
  test_c ();
  test_d ();
  test_e ();
  test_f ();
}
