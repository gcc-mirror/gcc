/* PR debug/64663 */
/* { dg-do compile } */
/* { dg-options "-O2 -g -w" } */

void
foo (void)
{
  int a[9];
  a[-8] = 0;
}

void
bar (void)
{
  int a[9];
  a[-9] = 0;
}
