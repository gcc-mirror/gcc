/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.2-a+crypto+nosha2" } */

int main ()
{
  return 0;
}

/* { dg-final { scan-assembler-times {\.arch armv8\.2\-a\+crc\+aes} 1 } } */

/* +crypto turns on +aes and +sha2, but +nosha2 disables +crypto.   */
