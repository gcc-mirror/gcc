/* Test register _Atomic compound literals.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

void
f ()
{
  (register _Atomic int) { 1 };
}
