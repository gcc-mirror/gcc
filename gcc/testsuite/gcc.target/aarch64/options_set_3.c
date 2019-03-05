/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.2-a+aes+sha2+crypto" } */

int main ()
{
  return 0;
}

/* { dg-final { scan-assembler-times {\.arch armv8\.2\-a\+crypto\+crc} 1 } } */

/* Check if smallest set is maintained when outputting. */
