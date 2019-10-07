/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=msp430" } { "" } } */
/* { dg-options "-mlarge" } */
/* { dg-final { scan-assembler ".mspabi_attribute 4, 2" } } */
/* { dg-final { scan-assembler ".mspabi_attribute 6, 2" } } */
/* { dg-final { scan-assembler ".mspabi_attribute 8, 2" } } */
/* { dg-final { scan-assembler ".gnu_attribute 4, 1" { target msp430_region_lower } } } */
/* { dg-final { scan-assembler ".gnu_attribute 4, 2" { target { ! msp430_region_lower } } } } */

int
main (void)
{
  while (1);
  return 0;
}
