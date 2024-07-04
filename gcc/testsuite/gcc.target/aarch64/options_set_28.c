/* { dg-do compile } */
/* { dg-additional-options "-march=armv9.3-a+nopredres+nomops" } */

int main ()
{
  return 0;
}

/* { dg-final { scan-assembler-times {\.arch armv9\.3\-a\+crc\+nopredres\+nomops\n} 1 } } */
