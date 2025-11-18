/* PR target/116815 */
/* { dg-do run { target int32 } } */
/* { dg-options "-O2" } */

[[gnu::always_inline]]
inline unsigned min (unsigned a, unsigned b)
{
  return (a < b) ? a : b;
}

[[gnu::always_inline]]
inline unsigned max (unsigned a, unsigned b)
{
  return (a > b) ? a : b;
}

[[gnu::noipa]] unsigned
umaxadd (unsigned a, unsigned b)
{
  return max (a + b, a);
}

[[gnu::noipa]] unsigned
umaxsub (unsigned a, unsigned b)
{
  return max (a - b, a);
}

[[gnu::noipa]] unsigned
uminadd (unsigned a, unsigned b)
{
  return min (a + b, a);
}

[[gnu::noipa]] unsigned
uminsub (unsigned a, unsigned b)
{
  return min (a - b, a);
}

int
main ()
{
  /* Overflows to 0x30000000.  */
  if (umaxadd (0x90000000, 0xa0000000) != 0x90000000)
    __builtin_abort ();

  if (uminadd (0x90000000, 0xa0000000) != 0x30000000)
    __builtin_abort ();

  /* Underflows to 0x60000000.  */
  if (umaxsub (0x00000000, 0xa0000000) != 0x60000000)
    __builtin_abort ();

  if (uminsub (0x00000000, 0xa0000000) != 0x00000000)
    __builtin_abort ();
}
