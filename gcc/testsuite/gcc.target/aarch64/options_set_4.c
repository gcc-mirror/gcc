/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.2-a+aes+sha2" } */

int main ()
{
  return 0;
}

/* { dg-final { scan-assembler-times {\.arch armv8\.2\-a\+crc\+crypto\n} 1 } } */

/* Check if individual bits that make up a grouping is specified that only the
   grouping is kept. */
