/* { dg-do link { target { { aarch64*-*-linux*} && native } } } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/aarch64/cpunative/info_31" } */
/* { dg-additional-options "-mcpu=native -mfix-cortex-a53-835769 -###" } */

int main()
{
  return 0;
}

/* { dg-message "-mno-fix-cortex-a53-835769" "note" { target *-*-* } 0 } */
/* { dg-excess-errors "" } */

/* Check that an Armv8-A core doesn't fall apart on extensions without midr
   values and that it enables optional features.  */
