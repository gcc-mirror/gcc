/* { dg-do compile { target { { aarch64*-*-linux*} && native } } } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/aarch64/cpunative/info_34" } */
/* { dg-additional-options "-march=armv8.8-a+sve -mcpu=native" } */

int main()
{
  return 0;
}

/* { dg-final { scan-assembler {\.arch armv8.8-a\+crc\+sve\n} } } */
/* { dg-excess-errors "" } */

/* Test a normal looking procinfo.  */
