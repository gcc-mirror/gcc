/* An unknown CPU without SVE2 uses the generic Armv8 CPU.  */
/* { dg-do compile { target { { aarch64*-*-linux* } && native } } } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/aarch64/cpunative/info_38" } */
/* { dg-additional-options "-mcpu=native -###" } */

int main ()
{
  return 0;
}

/* { dg-message "-mcpu=generic-armv8-a\\+dotprod\\+crc\\+crypto" "note" { target *-*-* } 0 } */
/* { dg-excess-errors "" } */
