/* { dg-error "unknown" "" {target "aarch64*-*-*" } } */
/* { dg-options "-O2 -march=dummy" } */

void f ()
{
  return;
}
