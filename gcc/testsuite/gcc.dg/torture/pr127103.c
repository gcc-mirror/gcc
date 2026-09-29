/* { dg-do run } */
/* minmax (a - c, b) + c must not become minmax (a, b + c) when a - c
   can wrap.  */

__attribute__((noipa)) unsigned
f_min (unsigned a, unsigned b, unsigned c)
{
  c &= 1;
  b &= 1;
  unsigned t = a - c;
  t = t < b ? t : b;
  return t + c;
}

__attribute__((noipa)) unsigned
f_max (unsigned a, unsigned b, unsigned c)
{
  c &= 1;
  b &= 1;
  unsigned t = a - c;
  t = t > b ? t : b;
  return t + c;
}

int
main (void)
{
  if (f_min (0, 0, 1) != 1)
    __builtin_abort ();
  if (f_max (0, 0, 1) != 0)
    __builtin_abort ();
  return 0;
}
