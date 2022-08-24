// PR c++/100252
// { dg-do run { target c++14 } }

struct A {
  int x;
  int y = x;
};

struct B {
  int x = 0;
  int y = A{x}.y;
};

static void
test_b (B b1 = B{}, B b2 = B{1}, B b3 = B{1, 2})
{
  if (b1.x != 0 || b1.y != b1.x)
    __builtin_abort();
  if (b2.x != 1 || b2.y != b2.x)
    __builtin_abort();
  if (b3.x != 1 || b3.y != 2)
    __builtin_abort();
}

struct C {
  int x = 0;
  int y = (true, A{x}.y) + (A{x}.y, 0);
};

static void
test_c (C c1 = C{}, C c2 = C{1}, C c3 = C{1, 2})
{
  if (c1.x != 0 || c1.y != c1.x)
    __builtin_abort();
  if (c2.x != 1 || c2.y != c2.x)
    __builtin_abort();
  if (c3.x != 1 || c3.y != 2)
    __builtin_abort();
}

struct D {
  int x = 0;
  int y = (A{x}.y);
};

static void
test_d (D d1 = D{}, D d2 = D{1}, D d3 = D{1, 2})
{
  if (d1.x != 0 || d1.y != d1.x)
    __builtin_abort();
  if (d2.x != 1 || d2.y != d2.x)
    __builtin_abort();
  if (d3.x != 1 || d3.y != 2)
    __builtin_abort();
}

struct E {
  int x = 0;
  int y = x ? A{x}.y : A{x}.y;
};

static void
test_e (E e1 = E{}, E e2 = E{1}, E e3 = E{1, 2})
{
  if (e1.x != 0 || e1.y != e1.x)
    __builtin_abort();
  if (e2.x != 1 || e2.y != e2.x)
    __builtin_abort();
  if (e3.x != 1 || e3.y != 2)
    __builtin_abort();
}

int
main ()
{
  test_b ();
  test_c ();
  test_d ();
  test_e ();
}
