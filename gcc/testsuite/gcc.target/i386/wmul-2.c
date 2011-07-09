/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target ia32 } */

void vec_mpy(int y[], const int x[], int scaler)
{
 int i;

 for (i = 0; i < 150; i++)
   y[i] += (((long long)scaler * x[i]) >> 31);
}

/* { dg-final { scan-assembler-times "imull" 1 } } */
