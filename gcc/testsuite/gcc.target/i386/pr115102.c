/* PR target/115102 */
/* { dg-do compile } */
/* { dg-options "-Os -march=x86-64 -dp" } */

unsigned int bswap8 (unsigned int val)
{
  return (val & 0xffff0000) | ((val & 0xff00) >> 8) | ((val & 0xff) << 8);
}

/* { dg-final { scan-assembler "bswaphisi2_lowpart" } } */
