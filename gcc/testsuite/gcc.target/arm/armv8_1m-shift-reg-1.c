/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-options "-O2 -mlittle-endian" } */
/* { dg-add-options arm_v8_1m_mve } */

long long longval2;
int intval2;

long long int
asrl_reg ()
{
 return (longval2 >> intval2);
}

long long unsigned int
lsll_reg (long long unsigned longval1, int intval1)
{
  return (longval1 << intval1);
}

/* { dg-final { scan-assembler "asrl\\tr\[0-9\], r\[0-9\], r\[0-9\]" } } */
/* { dg-final { scan-assembler "lsll\\tr\[0-9\], r\[0-9\], r\[0-9\]" } } */
