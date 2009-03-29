/* Test for bogus -Wsign-compare warnings that appeared when not
   folding operands before warning.  */
/* { dg-do compile } */
/* { dg-options "-Wsign-compare" } */

int
test_compare (int a, unsigned b)
{
  return (b > 8 * (a ? 4 : 8));
}

unsigned int
test_conditional (int a, unsigned b, int c)
{
  return (c ? b : 8 * (a ? 4 : 8));
}
