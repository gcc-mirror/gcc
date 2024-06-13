/* Test C2Y complex increment and decrement: allowed for C23 by default (not
   pedantic).  */
/* { dg-do compile } */
/* { dg-options "-std=c23" } */

_Complex float a;

void
f (void)
{
  a++;
  ++a;
  a--;
  --a;
}
