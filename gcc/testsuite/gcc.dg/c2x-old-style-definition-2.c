/* Test old-style function definitions not in C2x: errors.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

void
f (x) /* { dg-error "old-style function definition" } */
     int x;
{
}
