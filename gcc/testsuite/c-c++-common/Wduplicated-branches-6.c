/* PR c/64279 */
/* { dg-do compile } */
/* { dg-options "-Wduplicated-branches" } */

void
f (int i)
{
  if (i == 0)
    ;
  else if (i == 1)
    ;
}
