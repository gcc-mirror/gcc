/* PR c/4697
   Test whether operand has no effect warning is given for compound
   expression.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Wunused" } */

int b;
int foo (int a)
{
  a = a + 1, 5 * b; /* { dg-warning "right-hand operand of comma expression has no effect" } */
  return a;
}
