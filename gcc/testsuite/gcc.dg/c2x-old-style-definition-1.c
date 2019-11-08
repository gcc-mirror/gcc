/* Test old-style function definitions not in C2x: warnings.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x" } */

void
f (x) /* { dg-warning "old-style function definition" } */
     int x;
{
}
