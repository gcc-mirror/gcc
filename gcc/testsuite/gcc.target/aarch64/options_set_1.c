/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.2-a" } */

int main ()
{
  return 0;
}

/* { dg-final { scan-assembler-times {\.arch armv8\.2\-a\+crc\n} 1 } } */

/* Check to see if crc is output by default.  */
