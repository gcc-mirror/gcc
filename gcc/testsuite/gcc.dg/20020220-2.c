/* PR c/4697
   Test whether value computed not used warning is given for compound
   expression.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Wunused" } */

int b;
int foo (int a)
{
  a = a + 1, 5 * b;	/* { dg-warning "value computed is not used" } */
  return a;
}
