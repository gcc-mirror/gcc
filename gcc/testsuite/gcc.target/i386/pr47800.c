/* PR target/47800 */
/* { dg-do compile } */
/* { dg-options "-O2 -march=nocona" } */

int
foo (unsigned char *x, unsigned char *y)
{
  unsigned char a;
  for (a = 0; x < y; x++)
    if (a & 0x80)
      a = (unsigned char) (a << 1) + 1 + *x;
    else
      a = (unsigned char) (a << 1) + *x;
  return a;
}
