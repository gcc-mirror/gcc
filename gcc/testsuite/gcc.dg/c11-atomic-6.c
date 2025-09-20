/* Test ICE accessing _Atomic void object.  */
/* { dg-do compile } */
/* { dg-options "-std=c11" } */

extern _Atomic void x;

void
f (void)
{
  /* This has undefined behavior (lvalue conversion on an incomplete type) but
     should not produce an ICE.  */
  x;
}
