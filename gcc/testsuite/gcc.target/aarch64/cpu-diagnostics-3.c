/* { dg-error "unknown" "" {target "aarch64*-*-*" } } */
/* { dg-options "-O2 -mcpu=example-1+dummy" } */

void f ()
{
  return;
}
