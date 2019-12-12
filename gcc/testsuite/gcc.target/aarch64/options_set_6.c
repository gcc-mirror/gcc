/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.2-a+crypto+nosha2" } */

int main ()
{
  return 0;
}

/* { dg-final { scan-assembler-times {\.arch armv8\.2\-a\+crypto\+crc} 1 } } */

/* Group as a whole was requested to be turned on, crypto itself is a bit and so
   just turning off one feature can't turn it off.   */
