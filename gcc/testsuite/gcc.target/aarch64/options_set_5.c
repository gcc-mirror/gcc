/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.2-a+aes+sha2+nosha2" } */

int main ()
{
  return 0;
}

/* { dg-final { scan-assembler-times {\.arch armv8\.2\-a\+crc\+aes\n} 1 } } */

/* Check if turning off feature bits works correctly and grouping is no
   longer valid.   */
