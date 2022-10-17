/* PR c/101537 */
/* { dg-do compile } */
/* { dg-options "-Wconversion" } */

int
foo ()
{
  int aaa = 1;
  unsigned char bbb = 0;
  bbb |= aaa ? 1 : 0;
  return bbb;
}

int
bar (unsigned char x, int f)
{
  x |= f ? 1 : 0;
  return x;
}

int
baz (unsigned char x, int f)
{
  x = x | f ? 1 : 0;
  return x;
}
