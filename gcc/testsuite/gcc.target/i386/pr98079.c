/* PR target/98079 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -march=pentium3" } */

typedef __UINT8_TYPE__ uint8_t;

uint8_t foo (uint8_t x)
{
  if (x & 0x80)
    x = -x;

  return x;
}
