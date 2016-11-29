/* { dg-error "invalid feature" "" {target "aarch64*-*-*" } 0 } */
/* { dg-skip-if "do not override -mcpu" { *-*-* } { "-mcpu=*" } { "" } } */
/* { dg-options "-O2 -mcpu=cortex-a53+dummy" } */

void f ()
{
  return;
}
