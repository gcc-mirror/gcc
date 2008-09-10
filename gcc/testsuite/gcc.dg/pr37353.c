/* PR tree-optimization/37353 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

extern double exp (double);

#define A exp (arg);
#define B A A A A A A A A A A
#define C B B B B B B B B B B

void
foo (double arg)
{
  C
}
