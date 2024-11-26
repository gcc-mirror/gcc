/* Test ICE with atomic compound assignment with incomplete type on RHS (bug
   98195).  */
/* { dg-do compile } */
/* { dg-options "" } */

_Atomic int x;

void
f (void)
{
  x += f (); /* { dg-error "invalid use of void expression" } */
}
