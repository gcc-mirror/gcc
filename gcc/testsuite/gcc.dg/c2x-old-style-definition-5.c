/* Test old-style function definitions not in C2x: () does not warn
   with -Wold-style-definition.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -Wold-style-definition" } */

void
f ()
{
}
