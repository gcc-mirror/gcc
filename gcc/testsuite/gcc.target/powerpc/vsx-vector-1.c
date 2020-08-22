/* { dg-do compile } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -ftree-vectorize -mdejagnu-cpu=power7 -ffast-math" } */
/* { dg-final { scan-assembler "xvadddp" } } */
/* { dg-final { scan-assembler "xvsubdp" } } */
/* { dg-final { scan-assembler "xvmuldp" } } */
/* { dg-final { scan-assembler "xvdivdp" } } */
/* { dg-final { scan-assembler "xvmadd" } } */
/* { dg-final { scan-assembler "xvmsub" } } */
/* { dg-final { scan-assembler "xvsqrtdp" } } */
/* { dg-final { scan-assembler "xvcpsgndp" } } */
/* { dg-final { scan-assembler "xvrdpim" } } */
/* { dg-final { scan-assembler "xvrdpip" } } */
/* { dg-final { scan-assembler "xvrdpiz" } } */
/* { dg-final { scan-assembler "xvrdpic" } } */
/* { dg-final { scan-assembler "xvrdpi " } } */

#ifndef SIZE
#define SIZE 1024
#endif

double a[SIZE] __attribute__((__aligned__(32)));
double b[SIZE] __attribute__((__aligned__(32)));
double c[SIZE] __attribute__((__aligned__(32)));
double d[SIZE] __attribute__((__aligned__(32)));
double e[SIZE] __attribute__((__aligned__(32)));

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

extern double sqrt (double);
extern double floor (double);
extern double ceil (double);
extern double trunc (double);
extern double nearbyint (double);
extern double rint (double);
extern double copysign (double, double);

void
vector_sqrt (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = sqrt (b[i]);
}

void
vector_floor (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = floor (b[i]);
}

void
vector_ceil (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = ceil (b[i]);
}

void
vector_trunc (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = trunc (b[i]);
}

void
vector_nearbyint (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = nearbyint (b[i]);
}

void
vector_rint (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = rint (b[i]);
}

void
vector_copysign (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = copysign (b[i], c[i]);
}
