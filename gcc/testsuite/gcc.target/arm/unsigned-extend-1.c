/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned char foo (unsigned char c)
{
  return (c >= '0') && (c <= '9');
}
/* We shouldn't need any zero-extension idioms here.  */
/* { dg-final { scan-assembler-not "\t(uxtb|and|lsr|lsl)" { xfail arm_thumb1 } } } */
