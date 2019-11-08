/* Test old-style function definitions not in C2x: warnings disabled
   by -Wno-old-style-definition.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -Wno-old-style-definition" } */

void
f (x)
     int x;
{
}
