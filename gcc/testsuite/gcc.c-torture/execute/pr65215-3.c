/* PR tree-optimization/65215 */

struct S { unsigned long long l1 : 24, l2 : 8, l3 : 32; };

static inline unsigned int
foo (unsigned int x)
{
  return (x >> 24) | ((x >> 8) & 0xff00) | ((x << 8) & 0xff0000) | (x << 24);
}

__attribute__((noinline, noclone)) unsigned long long
bar (struct S *x)
{
  unsigned long long x1 = foo (((unsigned int) x->l1 << 8) | x->l2);
  unsigned long long x2 = foo (x->l3);
  return (x2 << 32) | x1;
}

int
main ()
{
  if (__CHAR_BIT__ != 8 || sizeof (unsigned int) != 4 || sizeof (unsigned long long) != 8)
    return 0;
  struct S s = { 0xdeadbeU, 0xefU, 0xfeedbea8U };
  unsigned long long l = bar (&s);
  if (foo (l >> 32) != s.l3
      || (foo (l) >> 8) != s.l1
      || (foo (l) & 0xff) != s.l2)
    __builtin_abort ();
  return 0;
}
