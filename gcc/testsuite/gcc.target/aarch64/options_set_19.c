/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.4-a+fp" } */

int main ()
{
  return 0;
}

/* { dg-final { scan-assembler {\.arch armv8\.4-a\+crc\n} } } */

 /* fp default, don't emit.  */
