/* { dg-do compile { target { { aarch64*-*-linux*} && native } } } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/aarch64/cpunative/info_26" } */
/* { dg-additional-options "-mcpu=native --save-temps " } */

int main()
{
  return 0;
}

/* { dg-final { scan-assembler {\.arch armv8.6-a\+rng\+sm4\+crc\+aes\+sha3\+fp16} } } */
/* Test that SoC with cores of 3 variants work.  */