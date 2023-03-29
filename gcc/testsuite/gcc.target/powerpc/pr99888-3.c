/* { dg-options "-fpatchable-function-entry=1" } */

/* Verify no errors on ELFv2, using command line option instead of
   function attribute.  */

extern int a;

int test (int b) {
  return a + b;
}

