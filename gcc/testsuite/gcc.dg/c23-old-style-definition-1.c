/* Test old-style function definitions not in C23: warnings.  */
/* { dg-do compile } */
/* { dg-options "-std=c23" } */

void
f (x) /* { dg-warning "old-style function definition" } */
     int x;
{
}
