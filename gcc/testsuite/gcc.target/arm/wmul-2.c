/* { dg-do compile } */
/* { dg-require-effective-target arm_dsp } */
/* { dg-options "-O1 -fexpensive-optimizations" } */

void vec_mpy(int y[], const short x[], short scaler)
{
 int i;

 for (i = 0; i < 150; i++)
   y[i] += ((scaler * x[i]) >> 31);
}

/* { dg-final { scan-assembler-times "smulbb" 1 } } */
