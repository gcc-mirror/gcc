/* Test for commas in constant expressions in #if: not permitted in C90
   but permitted in unevaluated subexpressions in C99.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do preprocess } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

#if (1, 2) /* { dg-error "comma" "evaluated comma" } */
#endif

#if 1 || (1, 2)
#endif
