/* { dg-error "missing" "" {target "aarch64*-*-*" } 0 } */
/* { dg-options "-O2 -march=+dummy" } */

void f ()
{
  return;
}
