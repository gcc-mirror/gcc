/* PR middle-end/112733 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

signed char a, c;
short b;

void
foo (void)
{
  signed char *e = &a;
  c = foo != 0;
  *e &= c;
  for (; b; --b)
    *e &= -128;
}
