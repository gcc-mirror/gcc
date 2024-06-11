/* PR target/112600 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "sbb|cmov" 4 } } */

unsigned char
add_sat_char (unsigned char x, unsigned char y)
{
  unsigned char z;
  return __builtin_add_overflow(x, y, &z) ? -1u : z;
}

unsigned short
add_sat_short (unsigned short x, unsigned short y)
{
  unsigned short z;
  return __builtin_add_overflow(x, y, &z) ? -1u : z;
}

unsigned int
add_sat_int (unsigned int x, unsigned int y)
{
  unsigned int z;
  return __builtin_add_overflow(x, y, &z) ? -1u : z;
}

unsigned long
add_sat_long (unsigned long x, unsigned long y)
{
  unsigned long z;
  return __builtin_add_overflow(x, y, &z) ? -1ul : z;
}
