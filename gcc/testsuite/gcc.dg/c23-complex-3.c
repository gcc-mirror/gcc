/* Test C2Y complex increment and decrement: allowed for C23 with
   -Wno-c23-c2y-compat.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors -Wno-c23-c2y-compat" } */

_Complex float a;

void
f (void)
{
  a++;
  ++a;
  a--;
  --a;
}
