/* PR c/85094 */
/* { dg-do compile } */
/* { dg-options "-O1 -Wduplicated-branches -g" } */

extern int g;

void
foo (int r)
{
  if (r < 64)
    g -= 48;
  else if (r < 80)	/* { dg-warning "this condition has identical branches" } */
    g -= 64 - 45;
  else
    g -= 80 - 61;
}
