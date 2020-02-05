/* { dg-do compile } */
/* { dg-additional-options "-Wno-implicit-function-declaration" } */

void foo (void)
{
  int i = actually_returns_void ();
}

void actually_returns_void (void) /* { dg-warning "conflicting types" } */
{
}
