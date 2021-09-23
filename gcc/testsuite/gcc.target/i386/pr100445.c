/* PR target/100445 */
/* { dg-do compile } */
/* { dg-options "-O3 -mxop" } */

int a, b[3];

void
foo (void)
{
  for (; a < 3; a++)
    b[a] = (a - 1) / 2;
}
