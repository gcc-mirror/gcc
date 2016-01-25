/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-O2 -ftree-vectorize -mcpu=power7 -ffast-math" } */
/* { dg-final { scan-assembler "xvaddsp" } } */
/* { dg-final { scan-assembler "xvsubsp" } } */
/* { dg-final { scan-assembler "xvmulsp" } } */
/* { dg-final { scan-assembler "xvdivsp" } } */
/* { dg-final { scan-assembler "vmadd" } } */
/* { dg-final { scan-assembler "xvmsub" } } */
/* { dg-final { scan-assembler "xvrsqrtesp" } } */
/* { dg-final { scan-assembler "xvcpsgnsp" } } */
/* { dg-final { scan-assembler "xvrspim" } } */
/* { dg-final { scan-assembler "xvrspip" } } */
/* { dg-final { scan-assembler "xvrspiz" } } */
/* { dg-final { scan-assembler "xvrspic" } } */
/* { dg-final { scan-assembler "xvrspi " } } */

#ifndef SIZE
#define SIZE 1024
#endif

float a[SIZE] __attribute__((__aligned__(32)));
float b[SIZE] __attribute__((__aligned__(32)));
float c[SIZE] __attribute__((__aligned__(32)));
float d[SIZE] __attribute__((__aligned__(32)));
float e[SIZE] __attribute__((__aligned__(32)));

void
vector_add (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = b[i] + c[i];
}

void
vector_subtract (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = b[i] - c[i];
}

void
vector_multiply (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = b[i] * c[i];
}

void
vector_multiply_add (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = (b[i] * c[i]) + d[i];
}

void
vector_multiply_subtract (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = (b[i] * c[i]) - d[i];
}

void
vector_divide (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = b[i] / c[i];
}

extern float sqrtf (float);
extern float floorf (float);
extern float ceilf (float);
extern float truncf (float);
extern float nearbyintf (float);
extern float rintf (float);
extern float copysignf (float, float);

void
vector_sqrt (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = sqrtf (b[i]);
}

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
vector_nearbyint (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = nearbyintf (b[i]);
}

void
vector_rint (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = rintf (b[i]);
}

void
vector_copysign (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = copysignf (b[i], c[i]);
}
