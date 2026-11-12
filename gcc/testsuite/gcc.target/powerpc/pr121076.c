/* { dg-do compile } */
/* { dg-options "-mcpu=power9 -O2" } */

unsigned short
swap (unsigned short in)
{
  return __builtin_bswap16 (in);
}

/* { dg-final { scan-assembler-times "slwi" 1 } } */
/* { dg-final { scan-assembler-times "rlwimi" 1 } } */
/* { dg-final { scan-assembler-times "rlwinm" 1 } } */
/* { dg-final { scan-assembler-not "or" } } */

