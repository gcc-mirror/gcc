/* PR middle-end/37870 */
/* { dg-do run } */
/* { dg-options "-O2" } */

unsigned int
foo (long double x)
{
  struct { char a[8]; unsigned int b:7; } c;
  __builtin_memcpy (&c, &x, sizeof (c));
  return c.b;
}

unsigned int
bar (long double x)
{
  union { struct { char a[8]; unsigned int b:7; } c; long double d; } u;
  u.d = x;
  return u.c.b;
}

int
main (void)
{
  if (foo (1.245L) != bar (1.245L)
      || foo (245.67L) != bar (245.67L)
      || foo (0.00567L) != bar (0.00567L))
    __builtin_abort ();
  return 0;
}
