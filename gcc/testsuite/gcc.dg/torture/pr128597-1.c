/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

/* Check that upper bits of a mask applied before a narrowing conversion
   are not taken as tests of extensions of the sign bit of the narrowed
   field.  */

__attribute__((noipa)) int
f (int x)
{
  int t = x & 0xc7ff3906;
  short s = t;
  if (s != 0)
    return 0;
  if (t < 0)
    return 0;
  return 1;
}

__attribute__((noipa)) int
g (long long x)
{
  long long t = x & 0xffff00007fffff00LL;
  int s = t;
  if (s != 0)
    return 0;
  if (t < 0)
    return 0;
  return 1;
}

int
main (void)
{
  /* Bit 15 of x is not in the mask, so t == 0.  */
  if (f (0x8000) != 1)
    __builtin_abort ();
  if (f (0x80000000) != 0 || f (0x100) != 0)
    __builtin_abort ();
  /* Bit 31 of x is set, but it is masked out.  */
  if (sizeof (long long) > sizeof (int) && g (0x80000000LL) != 1)
    __builtin_abort ();
  return 0;
}
