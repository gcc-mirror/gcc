/* PR c/80163 */
/* { dg-do compile { target int128 } } */
/* { dg-require-effective-target label_values } */

volatile int v;

__attribute__((noinline, noclone)) void
bar (void)
{
  v++;
  asm volatile ("" : : : "memory");
}

__attribute__((noinline, noclone)) __int128_t *
foo (unsigned long **p)
{
a:
  bar ();
b:
  bar ();
  static __int128_t d = (unsigned long) &&a - (unsigned long) &&b;
  static unsigned long e = (unsigned long) &&a - (unsigned long) &&b;
  *p = &e;
  return &d;
}

int
main ()
{
  __int128_t *p;
  unsigned long *q;
  p = foo (&q);
  if (*p != *q)
    __builtin_abort ();
  return 0;
}
