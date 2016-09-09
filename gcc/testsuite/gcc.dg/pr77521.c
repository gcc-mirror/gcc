/* PR c/77521 - %qc format directive should quote non-printable characters.
   Verify that non-printable characters in assembly constraints are quoted
   and not allowed to be printed raw.  */

void f (int a, int b)
{
  __asm__ ("combine %2, %0" : "=r" (a) : "0" (a), "\n" (b));   /* { dg-error "invalid punctuation .\\\\x0a. in constraint" } */
}
