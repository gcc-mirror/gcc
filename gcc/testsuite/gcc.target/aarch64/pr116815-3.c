/* { dg-do run } */
/* { dg-options "-O2" } */

/* PR middle-end/116815 */

/* Verify that the transformation gives correct results */

static inline unsigned __attribute__ ((always_inline))
min (unsigned a, unsigned b)
{
  return (a < b) ? a : b;
}

static inline unsigned __attribute__ ((always_inline))
max (unsigned a, unsigned b)
{
  return (a > b) ? a : b;
}

__attribute__ ((noipa)) unsigned
umaxadd (unsigned a, unsigned b)
{
  return max (a + b, a);
}

__attribute__ ((noipa)) unsigned
umaxsub (unsigned a, unsigned b)
{
  return max (a - b, a);
}

__attribute__ ((noipa)) unsigned
uminadd (unsigned a, unsigned b)
{
  return min (a + b, a);
}

__attribute__ ((noipa)) unsigned
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
