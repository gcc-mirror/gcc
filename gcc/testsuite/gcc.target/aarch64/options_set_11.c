/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.2-a+fp" } */

int main ()
{
  return 0;
}

/* { dg-final { scan-assembler {\.arch armv8\.2-a\+crc} } } */

 /* FP is default on, no need to pass on to assembler.  */
