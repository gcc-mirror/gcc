/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.2-a+fp16+fp" } */

int main ()
{
  return 0;
}

/* { dg-final { scan-assembler {\.arch armv8\.2-a\+crc\+fp16\n} } } */

 /* FP is part of FP16, don't emit it.  */
