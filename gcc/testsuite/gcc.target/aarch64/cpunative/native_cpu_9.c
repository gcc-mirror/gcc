/* { dg-do compile { target { { aarch64*-*-linux*} && native } } } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/aarch64/cpunative/info_9" } */
/* { dg-additional-options "-mcpu=native" } */

int main()
{
  return 0;
}

/* { dg-final { scan-assembler {\.arch armv8-a\+sve2-sm4\n} } } */

/* Test one here a feature that is a prefix of another is enabled.
   In this case sve is a prefix to svesm4, but sve2-sm4 should be
   enabled.  */
