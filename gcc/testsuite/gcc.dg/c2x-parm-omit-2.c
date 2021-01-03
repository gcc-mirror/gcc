/* Test omitted parameter names in C2x.  Warning test: there should be
   no warning for an unnamed parameter being unused.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors -Wall -Wextra" } */

int
f (int a, int, int c, int d) /* { dg-warning "unused parameter 'd'" } */
{
  return a + c;
}
