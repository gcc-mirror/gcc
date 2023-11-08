/* Test old-style function definitions not in C23: errors.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

void
f (x) /* { dg-error "old-style function definition" } */
     int x;
{
}
