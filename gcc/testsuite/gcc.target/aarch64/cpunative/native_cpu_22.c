/* { dg-do compile { target { { aarch64*-*-linux*} && native } } } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/aarch64/cpunative/info_22" } */
/* { dg-additional-options "-mcpu=native" } */

int main()
{
  return 0;
}

/* { dg-final { scan-assembler {\.arch armv8-a\+flagm2\+lse\+dotprod\+rdma\+crc\+fp16fml\+jscvt\+rcpc2\+frintts\+i8mm\+bf16\+sve2-aes\+sve2-bitperm\+sve2-sha3\+sve2-sm4\+sb\+ssbs\+pauth\n} } } */

/* Check that an Armv8-A core doesn't fall apart on extensions without midr
   values and that it enables optional features.  */
