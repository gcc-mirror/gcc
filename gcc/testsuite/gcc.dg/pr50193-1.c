/* PR 50193: ARM: ICE on a | (b << negative-constant) */
/* Ensure that the compiler doesn't ICE.  */

/* { dg-options "-O2" } */

int
foo(int a, int b)
{
  return a | (b << -3); /* { dg-warning "left shift count is negative" } */
}
