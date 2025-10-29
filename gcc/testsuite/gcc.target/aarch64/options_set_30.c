/* { dg-do compile } */
/* { dg-additional-options "-march=armv8-a+sve2+sve-aes+sve-bitperm+sve-sm4+sve-sha3" } */

int main ()
{
  return 0;
}

/* Make sure we prefer the sve2-X aliases over sve-X. */
/* { dg-final { scan-assembler {\.arch armv8-a\+sve2-aes\+sve2-bitperm\+sve2-sha3\+sve2-sm4\n} } } */
