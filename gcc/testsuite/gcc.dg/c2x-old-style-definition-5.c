/* Test old-style function definitions not in C23: () does not warn
   with -Wold-style-definition.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -Wold-style-definition" } */

void
f ()
{
}
