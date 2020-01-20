/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.1-m.main+mve -mfloat-abi=softfp" } */

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
