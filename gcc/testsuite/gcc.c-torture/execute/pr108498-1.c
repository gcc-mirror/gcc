/* PR tree-optimization/108498 */

struct A
{
  signed char a1;
  int a2;
};

struct B
{
  struct A b1;
  unsigned char b2:1, b3:1, b4:2, b5:1, b6:1, b7[4];
};

struct C
{
  unsigned char c1;
  char c2;
  signed char c3;
  unsigned char c4, c5[4], c6:1, c7:1, c8:1, c9:3, c10:1;
  struct A c11;
  struct B c12[3];
};

static inline struct C
foo (unsigned char a, unsigned b, int c, struct A d,
     unsigned e, struct B f, struct B g, struct B h)
{
  struct C x
    = { .c1 = b, .c2 = 0, .c3 = c, .c6 = a, .c4 = e, .c7 = 0,
        .c8 = 0, .c9 = 7, .c10 = 0, .c5 = {0, 1, 2, 3}, .c11 = d,
        .c12 = {f, g, h} };
  return x;
}

static inline struct A
bar (int a, int b)
{
  struct A x = { .a1 = a, .a2 = b };
  return x;
}

static inline struct B
baz (struct A b1)
{
  struct B x = { .b1 = b1, .b6 = 0, .b5 = 0, .b7 = {0, 1, 2, 3}, .b2 = 0 };
  return x;
}

struct C
qux (void)
{
  const struct B a = baz (bar (0, 0));
  struct C b;
  struct B c[2];
  struct A d = { 0, 1 };
  c[0].b1.a1 = 0;
  c[0].b1.a2 = 2;
  c[1].b1.a1 = 4;
  c[1].b1.a2 = 8;
  return foo (0, 2, -1, d, 3, c[0], c[1], a);
}

__attribute__((noipa)) void
corge (struct C *x)
{
  char buf[1024];
  __builtin_memset (buf, 0xaa, sizeof (buf));
  asm volatile ("" : : "r" (buf));
  __builtin_memset (x, 0x55, sizeof (struct C));
  asm volatile ("" : : "r" (x));
}

int
main ()
{
  struct C x;
  corge (&x);
  x = qux ();
  if (x.c6 || x.c9 != 7)
    __builtin_abort ();
}
