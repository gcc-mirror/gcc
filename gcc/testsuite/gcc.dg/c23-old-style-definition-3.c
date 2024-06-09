/* Test old-style function definitions not in C23: warnings disabled
   by -Wno-old-style-definition.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -Wno-old-style-definition" } */

void
f (x)
     int x;
{
}
