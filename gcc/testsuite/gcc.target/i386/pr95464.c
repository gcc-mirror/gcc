/* { dg-options "-O2" } */
/* { dg-do run { target { { *-*-linux* } && { ! ia32 } } } } */

struct S { unsigned a:1, b:1, c:1, d:1, e:14, f:14; };

__attribute__((noipa)) int
foo (struct S x)
{
  if (x.a != 0 || x.b != 1 || x.c != 0 || x.d != 1
      || x.e != 7239 || x.f != 6474)
    __builtin_abort ();
}

__attribute__((noipa)) void
bar (struct S x, struct S y)
{
  if (x.a != 0 || x.b != 1 || x.c != 0 || x.d != 1
      || x.e != 7239 || x.f != 6474)
    __builtin_abort ();
  if (y.a != 0 || y.b != 1 || y.c != 1 || y.d != 1
      || y.e != 16320 || y.f != 7315)
    __builtin_abort ();
}

__attribute__((noipa)) void
baz (struct S x)
{
  if (x.a != 1 || x.b != 1 || x.c != 1 || x.d != 1
      || x.e != 16320 || x.f != 7315)
    __builtin_abort ();
}

__attribute__((noipa)) void
qux (struct S x, struct S y, unsigned z)
{
  struct S a = x, b;
  for (unsigned i = 0; i < z; ++i)
    foo (x);
  if (x.a && x.e == 16)
    a.e = 32;
  b = a;
  b.c = y.c;
  b.e = y.e;
  b.f = y.f;
  bar (a, b);
  a = b;
  __asm volatile ("" : : : "ax", "bx", "cx", "dx", "si", "di",
#ifdef __OPTIMIZE__
			   "bp",
#endif
			   "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15");
  a.a = 1;
  a.c = 1;
  baz (a);
}

int
main ()
{
  struct S x = { 0, 1, 0, 1, 7239, 6474 };
  struct S y = { 1, 0, 1, 0, 16320, 7315 };
  qux (x, y, 1);
  return 0;
}
