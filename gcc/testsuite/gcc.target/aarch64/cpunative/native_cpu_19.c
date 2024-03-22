/* { dg-do compile { target { { aarch64*-*-linux*} && native } } } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/aarch64/cpunative/info_19" } */
/* { dg-additional-options "-mcpu=native" } */

int main()
{
  return 0;
}

/* { dg-final { scan-assembler {\.arch armv9-a\+crc\+i8mm\+bf16\+sve2-aes\+sve2-bitperm\+sve2-sha3\+sve2-sm4\+memtag\+profile\+nopauth\n} } } */

/* Test one that if the kernel doesn't report the availability of a mandatory
   feature that it has turned it off for whatever reason.  As such compilers
   should follow along. */
