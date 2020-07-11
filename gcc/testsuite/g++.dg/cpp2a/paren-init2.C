// PR c++/91363 - P0960R3: Parenthesized initialization of aggregates.
// { dg-do run { target c++20 } }

struct A {
  int i = 0;
  int j = 0;
};

struct B {
  A a;
  constexpr B() : a(1.1, 2) { }
};

struct C {
  int i;
};

struct E {
  C c;
  E() : c(1.2) { }
};

struct F {
  char a[4];
};

struct G {
  F f;
  G() : f("yew") { }
};

struct H {
  int i;
  int &&r;
};

int f() { return 42; }

struct I {
  H h;
  I() : h(1, f()) { }
};

I i;  // dangling ref to f():
      // {.i=1, .r=(int &) &TARGET_EXPR <D.2118, f ()>}

int
main ()
{
  B b;
  if (b.a.i != 1 || b.a.j != 2)
    __builtin_abort ();
  E e;
  if (e.c.i != 1)
    __builtin_abort ();
}
