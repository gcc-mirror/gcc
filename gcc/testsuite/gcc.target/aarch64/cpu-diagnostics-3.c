/* { dg-error "invalid feature" "" {target "aarch64*-*-*" } } */
/* { dg-options "-O2 -mcpu=cortex-a53+dummy" } */

void f ()
{
  return;
}
