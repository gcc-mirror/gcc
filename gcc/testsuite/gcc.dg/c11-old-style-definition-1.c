/* Test old-style function definitions not in C23: allowed in C11.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

void
f (x)
     int x;
{
}
