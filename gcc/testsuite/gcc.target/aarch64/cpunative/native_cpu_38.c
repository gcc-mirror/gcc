/* Check the inferred CPU as well as the ISA for an unknown SVE2 core.  */
/* { dg-do compile { target { { aarch64*-*-linux* } && native } } } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/aarch64/cpunative/info_37" } */
/* { dg-additional-options "-mcpu=native -###" } */

int main ()
{
  return 0;
}

/* { dg-message "-mcpu=generic-armv9-a\\+crc\\+crypto\\+sve2p1.*\\+nopredres" "note" { target *-*-* } 0 } */
/* { dg-excess-errors "" } */
