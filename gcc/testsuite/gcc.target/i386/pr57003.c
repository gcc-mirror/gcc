/* PR rtl-optimization/57003 */
/* { dg-do run { target { ! x32 } } } */
/* { dg-options "-O2" } */

#define N 2001
unsigned short *b, *c, *d;

__attribute__ ((noinline, noclone)) unsigned
bar (void)
{
  asm volatile ("" : : : "memory");
  return N;
}

__attribute__ ((noinline, noclone)) unsigned short *
baz (unsigned long x)
{
  if (x != N * sizeof (unsigned short) + 20)
    __builtin_abort ();
  asm volatile ("" : : : "memory");
  return d;
}

__attribute__ ((ms_abi, noinline, noclone)) void
foo (void)
{
  unsigned d;
  unsigned short *e;
  if ((d = bar ()))
    {
      e = baz (d * sizeof (unsigned short) + 20);
      __builtin_memcpy (e, b, d * sizeof (unsigned short));
      c = e;
    }
}

int
main ()
{
  unsigned short a[2 * N];
  int i;
  for (i = 0; i < 2 * N; i++)
    a[i] = i + 1;
  b = a;
  d = a + N;
  asm volatile ("" : : : "memory");
  foo ();
  for (i = 0; i < N; i++)
    if (a[i] != i + 1 || a[i + N] != i + 1)
      __builtin_abort ();
  if (c != a + N)
    __builtin_abort ();
  return 0;
}
