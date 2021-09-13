/* PR target/99600 */
/* { dg-do compile } */
/* { dg-options "-O2 -march=atom" } */

char a, b;
long c;

long
foo (void)
{
  if (a)
    c = b == 1 ? 1 << 3 : 1 << 2;
  else
    c = 0;
  return 0;
}
