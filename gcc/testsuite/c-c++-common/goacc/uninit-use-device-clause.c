/* Test fails due to PR70388.  */
/* { dg-do compile } */
/* { dg-excess-errors "PR70388" { xfail *-*-* } } */
/* { dg-additional-options "-Wuninitialized" } */

void
foo (void)
{
  int i;

#pragma acc host_data use_device(i) /* { dg-warning "is used uninitialized" "" { xfail *-*-* } } */
  {
  }
}
