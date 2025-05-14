/* { dg-do compile } */
/* { dg-options "-O2 -mavx2" } */

void lshift(unsigned short *x, unsigned char amount)
{
  if (amount > 15)
    __builtin_unreachable();

  for (int i = 0; i < 16; i++)
    x[i] <<= amount;
}

/* { dg-final { scan-assembler "vpsllw" } } */
