// PR tree-optimization/102586
// { dg-options "-Wno-inaccessible-base" }

struct C0 {};
struct C1 {};
struct C2 : C1, virtual C0 {};
struct C3 : virtual C2, C1 { virtual int foo () { return 1; } };
struct C4 : virtual C3, C1 { virtual int foo () { return 2; } };
struct C5 : C4 { virtual int foo () { return 3; } };
struct C6 { char c; };
struct C7 : virtual C6, virtual C3, C1 { virtual int foo () { return 4; } };
struct C8 : C7 { virtual int foo () { return 5; } };

__attribute__((noipa)) int
bar (C5 *p)
{
  return p->foo ();
}

__attribute__((noipa)) int
baz (C3 *p)
{
  return p->foo ();
}

__attribute__((noipa)) int
qux (C8 *p)
{
  return p->foo ();
}

int
main ()
{
  C5 c5;
  C8 c8;
  c8.c = 42;
  __builtin_clear_padding (&c5);
  __builtin_clear_padding (&c8);
  if (bar (&c5) != 3 || baz (&c5) != 3)
    __builtin_abort ();
  if (qux (&c8) != 5 || baz (&c8) != 5 || c8.c != 42)
    __builtin_abort ();
}
