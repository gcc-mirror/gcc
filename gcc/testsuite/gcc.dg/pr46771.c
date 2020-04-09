/* PR debug/46771 */
/* { dg-do compile } */
/* { dg-options "-O -ftree-vectorize -fcompare-debug" } */

unsigned char v[1600];

unsigned char
foo (unsigned char x)
{
  int i;
  unsigned char a = x;
  unsigned char b = x;
  for (i = 0; i < 1600; i++)
    a = a < v[i] ? v[i] : a;
  for (i = 0; i < 1600; i++)
    b = b > v[i] ? v[i] : b;
  return a - b;
}
