/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-O2 -ftree-vectorize -mcpu=power6 -m64 -maltivec" } */
/* { dg-final { scan-assembler "vsel" } } */
/* { dg-final { scan-assembler "vrfim" } } */
/* { dg-final { scan-assembler "vrfip" } } */
/* { dg-final { scan-assembler "vrfiz" } } */

#ifndef SIZE
#define SIZE 1024
#endif

float a[SIZE] __attribute__((__aligned__(32)));
float b[SIZE] __attribute__((__aligned__(32)));
float c[SIZE] __attribute__((__aligned__(32)));
float d[SIZE] __attribute__((__aligned__(32)));
float e[SIZE] __attribute__((__aligned__(32)));

extern float floorf (float);
extern float ceilf (float);
extern float truncf (float);
extern float copysignf (float, float);

void
vector_floor (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = floorf (b[i]);
}

void
vector_ceil (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = ceilf (b[i]);
}

void
vector_trunc (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = truncf (b[i]);
}

void
vector_copysign (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = copysignf (b[i], c[i]);
}
