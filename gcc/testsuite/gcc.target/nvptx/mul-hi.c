/* { dg-do assemble } */
/* { dg-options "-O2 -save-temps" } */

short smulhi3_highpart(short x, short y)
{
  return ((int)x * (int)y) >> 16;
}

int smulsi3_highpart(int x, int y)
{
  return ((long)x * (long)y) >> 32;
}

/* { dg-final { scan-assembler-times "mul.hi.s16" 1 } } */
/* { dg-final { scan-assembler-times "mul.hi.s32" 1 } } */
