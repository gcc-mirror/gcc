/* { dg-do compile { target { { aarch64*-*-linux*} && native } } } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/aarch64/cpunative/info_15" } */
/* { dg-additional-options "-mcpu=native" } */

int main()
{
  return 0;
}

/* { dg-final { scan-assembler {\.arch armv8-a\+sve2-sm4} } } */

/* Test one where the bounary of buffer size would cut off and leave
   a valid feature in the first full buffer.  e.g. this will cut off at
   sve leaving 2-sm4 to yet be read.  Check that this doesn't enable
   +sve by mistake.  */
