/* Check that using -mdiv=call-fp compiles without fuzz.  */
/* { dg-do compile }  */
/* { dg-additional-options "-mdiv=call-fp" }  */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*" } { "" } }  */

int
test (int a, int b, int c, int d)
{
  return (a / b) + c + d;
}
