/* PR tree-optimization/108692 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

__attribute__((noipa)) int
foo (signed char *x, signed char *y, int n)
{
  int i, r = 0;
  signed char a, b;
  for (i = 0; i < n; i++)
    {
      a = x[i];
      b = y[i];
      int c = (unsigned char) a - (unsigned char) b;
      r = r + (c < 0 ? -c : c);
    }
  return r;
}

int
main ()
{
  signed char x[64] = {}, y[64] = {};
  if (__CHAR_BIT__ != 8 || __SIZEOF_INT__ != 4)
    return 0;
  x[32] = -128;
  y[32] = 1;
  if (foo (x, y, 64) != 127)
    __builtin_abort ();
  return 0;
}
