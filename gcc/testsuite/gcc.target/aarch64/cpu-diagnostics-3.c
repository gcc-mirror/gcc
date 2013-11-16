/* { dg-error "unknown" "" {target "aarch64*-*-*" } } */
/* { dg-options "-O2 -mcpu=cortex-a53+dummy" } */

void f ()
{
  return;
}
