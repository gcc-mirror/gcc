/* { dg-error "missing" "" {target "aarch64*-*-*" } } */
/* { dg-options "-O2 -mcpu=cortex-a53+no" } */

void f ()
{
  return;
}
