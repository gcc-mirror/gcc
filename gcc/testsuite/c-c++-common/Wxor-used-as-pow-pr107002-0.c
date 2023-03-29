/* Regression test for ICE seen in -Wxor-used-as-pow with
   -ftrack-macro-expansion=0 in source-printing (fix-it-hints,
   specifically).  */

/* { dg-options "-ftrack-macro-expansion=0 -fdiagnostics-show-caret" } */

#define test(lower, higher, a, b, c, d)					\
  char test##line[ (a higher b lower c higher d) == 0 ? -1 : 1];
test (|, ^, 1, 2, 2, 1)
