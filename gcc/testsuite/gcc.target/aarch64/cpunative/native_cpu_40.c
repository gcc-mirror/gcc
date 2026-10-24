/* Unknown heterogeneous cores use the detected features for the fallback.  */
/* { dg-do compile { target { { aarch64*-*-linux* } && native } } } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/aarch64/cpunative/info_35" } */
/* { dg-additional-options "-mcpu=native -###" } */

int main ()
{
  return 0;
}

/* { dg-message "-mcpu=generic-armv9-a\\+crc\\+crypto.*\\+nopredres" "note" { target *-*-* } 0 } */
/* { dg-excess-errors "" } */
