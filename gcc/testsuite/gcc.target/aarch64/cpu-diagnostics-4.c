/* { dg-error "missing" "" {target "aarch64*-*-*" } } */
/* { dg-skip-if "do not override -mcpu" { *-*-* } { "-mcpu=*" } { "" } } */
/* { dg-options "-O2 -mcpu=+dummy" } */

void f ()
{
  return;
}
