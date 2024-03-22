/* { dg-do compile { target { { aarch64*-*-linux*} && native } } } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/aarch64/cpunative/info_18" } */
/* { dg-additional-options "-mcpu=native" } */

int main()
{
  return 0;
}

/* { dg-final { scan-assembler {\.arch armv8.6-a\+rng\+crc\+aes\+sha3\+fp16\+nopauth\n} } } */

/* Test one where the boundary of buffer size would overwrite the last
   character read when stitching the fgets-calls together.  With the
   test data provided, this would truncate the 'sha512' into 'ha512'
   (dropping the 'sha3' feature). */
