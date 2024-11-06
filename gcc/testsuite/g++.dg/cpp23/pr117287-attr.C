// P1774R8 - Portable assumptions
// { dg-do run { target c++23 } }
// { dg-options "-O2 --param=logical-op-non-short-circuit=0" }
// Test the we can optimize based on conditions in assume.

static inline bool
foo (unsigned x)
{
  return x == 4 || x == 5 || x == 9 || x == 10;
}

int v;

[[gnu::noipa]] void
bar (const char *p)
{
  if (p[0] != (v ? 'a' : 'b') || p[1])
    __builtin_abort ();
}

[[gnu::noipa]] void
baz (unsigned x)
{
  bool a = x == 5;
  [[assume (foo (x))]];
  bar (a ? "a" : "b");
}

int
main ()
{
  baz (4);
  v = 1;
  baz (5);
  v = 0;
  baz (9);
  baz (10);
}
