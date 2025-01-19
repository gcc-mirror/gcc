/* { dg-do link { target { { aarch64*-*-linux*} && native } } } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/aarch64/cpunative/info_30" } */
/* { dg-additional-options "-mcpu=native -mfix-cortex-a53-835769 -###" } */

int main()
{
  return 0;
}

/* { dg-message "-mfix-cortex-a53-835769" "note" { target *-*-* } 0 } */
/* { dg-message "--fix-cortex-a53-835769" "note" { target *-*-* } 0 } */
/* { dg-excess-errors "" } */

/* Test a normal looking procinfo.  */
