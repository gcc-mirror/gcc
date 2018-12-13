/* { dg-do compile } */
/* { dg-options "-O1 -ftrapv" } */

unsigned int
foo (unsigned int *x, const unsigned int *y, int z, unsigned int w)
{
  unsigned int a, b, c, s;
  int j;
  j = -z;
  x -= j;
  y -= j;
  a = 0;
  do
    {
      asm volatile ("" : "=d" (b), "=d" (c) : "r" (y[j]), "d" (w)); /* { dg-error "'asm' operand has impossible constraints" } */
      c += a;
      a = (c < a) + b;
      s = x[j];
      c = s + c;
      a += (c < s);
      x[j] = c;
    }
  while (++j != 0);
  return a;
}
