/* { dg-do compile { target { { aarch64*-*-linux*} && native } } } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/aarch64/cpunative/info_24" } */
/* { dg-additional-options "-mcpu=native --save-temps " } */

int main()
{
  return 0;
}

/* { dg-final { scan-assembler {\.arch armv8-a\+dotprod\+crc\+crypto\+rcpc3} } } */
/* Test one where rcpc3 is available and so should be emitted.  */
