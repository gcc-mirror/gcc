/* { dg-do compile } */
/* { dg-additional-options "-Ofast -mcpu=neoverse-v2" } */

#define iterations 100000
#define LEN_1D 32000

float a[LEN_1D], b[LEN_1D];

float
s4115 (int *ip)
{
    float sum = 0.;
    for (int i = 0; i < LEN_1D; i++)
      {
        sum += a[i] * b[ip[i]];
      }
    return sum;
}

/* { dg-final { scan-assembler-not {\s+st1w\t} } } */
