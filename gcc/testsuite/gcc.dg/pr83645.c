/* PR debug/83645 */
/* { dg-do compile } */
/* { dg-options "-O2 -g -fno-var-tracking" } */

int a, b, c[1];

void
foo (void)
{
  int i = 0;
  b = a;
  for (;;)
    c[i++] = 7;
}
