/* { dg-error "missing" "" {target "aarch64*-*-*" } } */
/* { dg-skip-if "do not override -mcpu" { *-*-* } { "-mcpu=*" } { "" } } */
/* { dg-options "-O2 -mcpu=cortex-a53+no" } */

void f ()
{
  return;
}
