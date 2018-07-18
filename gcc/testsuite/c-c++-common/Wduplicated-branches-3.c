/* PR c/64279 */
/* { dg-do compile } */
/* { dg-options "-Wduplicated-branches" } */

extern int *g;

void
f (short int i)
{
  if (i == 0) /* { dg-warning "this condition has identical branches" } */
    *g = (int) i;
  else
    *g = (int) i;

  if (i == 1)
    *g = (unsigned char) i;
  else
    *g = (signed char) i;
}
