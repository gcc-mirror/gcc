/* Check that using -mdiv=call-fp compiles without fuzz.  */
/* { dg-do compile }  */
/* { dg-additional-options "-mdiv=call-fp" }  */

int
test_0 (int a, int b, int c, int d)
{
  return (a / b) + c + d;
}

unsigned int
test_1 (unsigned int a, unsigned int b, unsigned int c, unsigned int d)
{
  return (a / b) + c + d;
}
