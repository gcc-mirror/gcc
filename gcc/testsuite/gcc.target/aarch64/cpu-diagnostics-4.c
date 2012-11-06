/* { dg-error "missing" "" {target "aarch64*-*-*" } } */
/* { dg-options "-O2 -mcpu=+dummy" } */

void f ()
{
  return;
}
