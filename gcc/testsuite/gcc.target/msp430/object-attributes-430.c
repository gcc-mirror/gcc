/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=msp430x" "-mlarge" } { "" } } */
/* { dg-options "-mcpu=msp430" } */
/* { dg-final { scan-assembler ".mspabi_attribute 4, 1" } } */
/* { dg-final { scan-assembler ".mspabi_attribute 6, 1" } } */
/* { dg-final { scan-assembler ".mspabi_attribute 8, 1" } } */
/* { dg-final { scan-assembler ".gnu_attribute 4, 1" } } */

int
main (void)
{
  while (1);
  return 0;
}
