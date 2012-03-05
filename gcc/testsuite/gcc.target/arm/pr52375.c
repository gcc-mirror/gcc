/* PR target/52375 */
/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-march=armv7-a -mfloat-abi=hard -mfpu=neon -O -ftree-vectorize" } */

struct C { int c, d; };

unsigned
foo (struct C *p)
{
  unsigned int b = 0, i;
  for (i = 0; i < 64; i++)
    b |= 0x80000000U >> p[i].c;
  return b;
}
