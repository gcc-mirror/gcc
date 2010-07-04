/* { dg-do compile } */
/* { dg-options "-O2 -march=armv6t2" } */

void vec_mpy(int y[], const short x[], short scaler)
{
 int i;

 for (i = 0; i < 150; i++)
   y[i] += ((scaler * x[i]) >> 31);
}

/* { dg-final { scan-assembler-times "smulbb" 1 } } */
