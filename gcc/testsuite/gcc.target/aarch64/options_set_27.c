/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.2-a+aes+sha3" } */

int main ()
{
  return 0;
}

/* { dg-final { scan-assembler-times {\.arch armv8\.2\-a\+crc\+aes\+sha3\n} 1 } } */
