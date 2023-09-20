/* { dg-do compile { target { { aarch64*-*-linux*} && native } } } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/aarch64/cpunative/info_20" } */
/* { dg-additional-options "-mcpu=native" } */

int main()
{
  return 0;
}

/* { dg-final { scan-assembler {\.arch armv9-a\+crc\+i8mm\+bf16\+sve2-aes\+sve2-bitperm\+sve2-sha3\+sve2-sm4\+memtag\+profile\n} } } */

/* Check whether features that don't have a midr name during detection are
   correctly ignored.  These features shouldn't affect the native detection.
   This particular test checks that predres is not turned off during
   detection.   */
