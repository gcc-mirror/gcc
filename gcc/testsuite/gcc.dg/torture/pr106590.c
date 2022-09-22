/* PR rtl-optimization/106590 } */
/* { dg-do run } */
/* { dg-additional-options "-mtune=skylake" { target { i?86-*-* x86_64-*-* } } } */

typedef struct A { short a; } A;
typedef A *B;
typedef struct C { int c, d; } C;
typedef C *D;

B
foo (void)
{
  static A r = { .a = 1 };
  return &r;
}

D
bar (void)
{
  static C r = { .c = 1, .d = 23 };
  return &r;
}

static inline int __attribute__((always_inline))
baz (short a)
{
  int e = 1, f;
  short g;
  D h;

  switch (a)
    {
    case 1:
      f = 23;
      g = 1;
      break;
    case 2:
      f = 20;
      g = 2;
      break;
    }

  h = bar ();

  if (h->d != f || h->c != g)
    __builtin_abort ();
  return e;
}

int
qux (void)
{
  B i = foo ();
  int e = 1;

  switch (i->a)
    {
    case 1:
    case 2:
      e = baz (i->a);
      break;
    case 3:
      e = 0;
      break;
    }

  return e;
}

int
main ()
{
  qux ();
  return 0;
}
