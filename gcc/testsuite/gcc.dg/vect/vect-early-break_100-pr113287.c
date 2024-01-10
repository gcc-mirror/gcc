/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target bitint } */

__attribute__((noipa)) void
bar (unsigned long *p)
{
  __builtin_memset (p, 0, 142 * sizeof (unsigned long));
  p[17] = 0x50000000000UL;
}

__attribute__((noipa)) int
foo (void)
{
  unsigned long r[142];
  bar (r);
  unsigned long v = ((long) r[0] >> 31);
  if (v + 1 > 1)
    return 1;
  for (unsigned long i = 1; i <= 140; ++i)
    if (r[i] != v)
      return 1;
  unsigned long w = r[141];
  if ((unsigned long) (((long) (w << 60)) >> 60) != v)
    return 1;
  return 0;
}

int
main ()
{
  if (foo () != 1)
    __builtin_abort ();
}
