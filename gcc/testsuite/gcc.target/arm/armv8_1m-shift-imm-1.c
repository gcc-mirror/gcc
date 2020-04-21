/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-options "-O2 -mlittle-endian" } */
/* { dg-add-options arm_v8_1m_mve } */

long long longval1;
long long unsigned longval2;

long long int
asrl_imm ()
{
 return (longval1 >> 14);
}

long long unsigned int
lsrl_imm ()
{
 return (longval2 >> 14);
}

long long int
lsll_imm (long long int longval3)
{
  return (longval3 << 14);
}

/* { dg-final { scan-assembler "asrl\\tr\[0-9\], r\[0-9\], #14" } } */
/* { dg-final { scan-assembler "lsrl\\tr\[0-9\], r\[0-9\], #14" } } */
/* { dg-final { scan-assembler "lsll\\tr\[0-9\], r\[0-9\], #14" } } */
