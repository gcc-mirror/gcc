/* { dg-do run } */
/* { dg-require-effective-target lp64 } */

unsigned long int
foo (signed char b, signed char e)
{
  return ((2ULL * b) * (e * 13)) * (32 << 24);
}

int
main ()
{
  if (__CHAR_BIT__ == 8
      && sizeof (int) == 4
      && sizeof (long long) == 8
      && foo (-60, 1) != 0xffffff3d00000000ULL)
    __builtin_abort ();
  return 0;
}
