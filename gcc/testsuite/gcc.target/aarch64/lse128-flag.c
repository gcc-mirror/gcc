/* { dg-do compile { target { aarch64*-*-*} } } */
/* { dg-additional-options "-march=armv9.4-a+lse128" } */

int main()
{
  return 0;
}

/* { dg-final { scan-assembler {\.arch armv9\.4-a\+crc\+lse128} } } */
/* Test a normal looking procinfo.  */
