/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=msp430" "-mdata-region=lower" } { "" } } */
/* { dg-options "-mlarge -mdata-region=none" } */
/* { dg-final { scan-assembler ".mspabi_attribute 4, 2" } } */
/* { dg-final { scan-assembler ".mspabi_attribute 6, 2" } } */
/* { dg-final { scan-assembler ".mspabi_attribute 8, 2" } } */
/* { dg-final { scan-assembler ".gnu_attribute 4, 2" } } */

int
main (void)
{
  while (1);
  return 0;
}
