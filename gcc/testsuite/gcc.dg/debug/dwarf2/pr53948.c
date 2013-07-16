/* Test that we have line information for the line
   with local variable initializations.  */
/* { dg-options "-O0 -g -dA" } */
/* { dg-final { scan-assembler ".loc 1 8 0|\[#/!\]\[ \t\]+line 8" } } */


int f (register int a, register int b) {
  register int x = b, y = a;
  return x + y; }

