/* Test for restrict: in C99 only.  Test handling of arrays of restricted
   pointers.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

typedef int *ipa[2];

int *restrict x[2];
restrict ipa y;

void f(int *restrict a[2], restrict ipa b, int *restrict c[restrict]);
