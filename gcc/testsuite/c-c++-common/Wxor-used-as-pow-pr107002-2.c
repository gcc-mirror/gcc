/* { dg-options "-ftrack-macro-expansion=2 -fdiagnostics-show-caret" } */

#define test(lower, higher, a, b, c, d)					\
  char test##line[ (a higher b lower c higher d) == 0 ? -1 : 1];
test (|, ^, 1, 2, 2, 1)
