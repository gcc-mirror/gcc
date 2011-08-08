/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O3 -ftree-vectorize -mcpu=power7 -ffast-math -ffp-contract=off" } */
/* { dg-final { scan-assembler-times "xvmadd" 2 } } */
/* { dg-final { scan-assembler-times "xsmadd\|fmadd\ " 1 } } */
/* { dg-final { scan-assembler-times "fmadds" 1 } } */
/* { dg-final { scan-assembler-times "xvmsub" 2 } } */
/* { dg-final { scan-assembler-times "xsmsub\|fmsub\ " 1 } } */
/* { dg-final { scan-assembler-times "fmsubs" 1 } } */
/* { dg-final { scan-assembler-times "xvnmadd" 2 } } */
/* { dg-final { scan-assembler-times "xsnmadd\|fnmadd\ " 1 } } */
/* { dg-final { scan-assembler-times "fnmadds" 1 } } */
/* { dg-final { scan-assembler-times "xvnmsub" 2 } } */
/* { dg-final { scan-assembler-times "xsnmsub\|fnmsub\ " 1 } } */
/* { dg-final { scan-assembler-times "fnmsubs" 1 } } */

/* Only the functions calling the bulitin should generate an appropriate (a *
   b) + c instruction.  */

double
builtin_fma (double b, double c, double d)
{
  return __builtin_fma (b, c, d);			/* xsmadd{a,m}dp */
}

double
builtin_fms (double b, double c, double d)
{
  return __builtin_fma (b, c, -d);			/* xsmsub{a,b}dp */
}

double
builtin_fnma (double b, double c, double d)
{
  return - __builtin_fma (b, c, d);			/* xsnmadd{a,b}dp */
}

double
builtin_fnms (double b, double c, double d)
{
  return - __builtin_fma (b, c, -d);			/* xsnmsub{a,b}dp */
}

float
builtin_fmaf (float b, float c, float d)
{
  return __builtin_fmaf (b, c, d);			/* fmadds */
}

float
builtin_fmsf (float b, float c, float d)
{
  return __builtin_fmaf (b, c, -d);			/* fmsubs */
}

float
builtin_fnmaf (float b, float c, float d)
{
  return - __builtin_fmaf (b, c, d);			/* fnmadds */
}

float
builtin_fnmsf (float b, float c, float d)
{
  return - __builtin_fmaf (b, c, -d);			/* fnmsubs */
}

double
normal_fma (double b, double c, double d)
{
  return (b * c) + d;					/* fmul/fadd */
}

float
normal_fmaf (float b, float c, float d)
{
  return (b * c) + d;					/* fmuls/fadds */
}

#ifndef SIZE
#define SIZE 1024
#endif

double vda[SIZE] __attribute__((__aligned__(32)));
double vdb[SIZE] __attribute__((__aligned__(32)));
double vdc[SIZE] __attribute__((__aligned__(32)));
double vdd[SIZE] __attribute__((__aligned__(32)));

float vfa[SIZE] __attribute__((__aligned__(32)));
float vfb[SIZE] __attribute__((__aligned__(32)));
float vfc[SIZE] __attribute__((__aligned__(32)));
float vfd[SIZE] __attribute__((__aligned__(32)));

void
vector_fma (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    vda[i] = __builtin_fma (vdb[i], vdc[i], vdd[i]);	/* xvmadd{a,m}dp */
}

void
vector_fms (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    vda[i] = __builtin_fma (vdb[i], vdc[i], -vdd[i]);	/* xvmsub{a,m}dp */
}

void
vector_fnma (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    vda[i] = - __builtin_fma (vdb[i], vdc[i], vdd[i]);	/* xvnmadd{a,m}dp */
}

void
vector_fnms (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    vda[i] = - __builtin_fma (vdb[i], vdc[i], -vdd[i]);	/* xvnmsub{a,m}dp */
}

void
vector_fmaf (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    vfa[i] = __builtin_fmaf (vfb[i], vfc[i], vfd[i]);	/* xvmadd{a,m}sp */
}

void
vector_fmsf (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    vfa[i] = __builtin_fmaf (vfb[i], vfc[i], -vfd[i]);	/* xvmsub{a,m}sp */
}

void
vector_fnmaf (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    vfa[i] = - __builtin_fmaf (vfb[i], vfc[i], vfd[i]);	/* xvnmadd{a,m}sp */
}

void
vector_fnmsf (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    vfa[i] = - __builtin_fmaf (vfb[i], vfc[i], -vfd[i]); /* xvnmsub{a,m}sp */
}

void
vnormal_fma (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    vda[i] = (vdb[i] * vdc[i]) + vdd[i];		/* xvmadd{a,m}dp */
}

void
vnormal_fmaf (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    vfa[i] = (vfb[i] * vfc[i]) + vfd[i];		/* xvmadd{a,m}sp */
}
