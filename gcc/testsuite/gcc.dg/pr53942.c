/* PR rtl-optimization/53942 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-mtune=pentium2" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */

struct S
{
  unsigned short w[3];
  unsigned int x, y;
};

struct S *baz (void);

__attribute__ ((noinline))
static unsigned char
foo (struct S *x, unsigned char y)
{
  unsigned char c = 0;
  unsigned char v = x->w[0];
  c |= v;
  v = ((x->w[1]) & (1 << y)) ? 1 : 0;
  c |= v << 1;
  v = ((x->w[2]) & 0xff) & (1 << y);
  c |= v << 2;
  return c;
}

void
bar (void)
{
  struct S *s = baz ();
  s->x = foo (s, 6);
  s->y = foo (s, 7);
}
