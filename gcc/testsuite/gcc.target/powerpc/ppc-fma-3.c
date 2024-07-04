/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O3 -ftree-vectorize -mdejagnu-cpu=power6 -maltivec -ffast-math -fno-unroll-loops" } */
/* { dg-require-effective-target powerpc_altivec } */
/* { dg-require-effective-target powerpc_fprs } */
/* { dg-final { scan-assembler-times "vmaddfp" 2 } } */
/* { dg-final { scan-assembler-times "fmadd " 2 } } */
/* { dg-final { scan-assembler-times "fmadds" 2 } } */
/* { dg-final { scan-assembler-times "fmsub " 1 } } */
/* { dg-final { scan-assembler-times "fmsubs" 1 } } */
/* { dg-final { scan-assembler-times "fnmadd " 1 } } */
/* { dg-final { scan-assembler-times "fnmadds" 1 } } */
/* { dg-final { scan-assembler-times "fnmsub " 1 } } */
/* { dg-final { scan-assembler-times "fnmsubs" 1 } } */

/* All functions should generate an appropriate (a * b) + c instruction
   since -mfused-madd is on by default.  */

double
builtin_fma (double b, double c, double d)
{
  return __builtin_fma (b, c, d);			/* fmadd */
}

double
builtin_fms (double b, double c, double d)
{
  return __builtin_fma (b, c, -d);			/* fmsub */
}

double
builtin_fnma (double b, double c, double d)
{
  return - __builtin_fma (b, c, d);			/* fnmadd */
}

double
builtin_fnms (double b, double c, double d)
{
  return - __builtin_fma (b, c, -d);			/* fnmsub */
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
  return (b * c) + d;					/* fmadd */
}

float
normal_fmaf (float b, float c, float d)
{
  return (b * c) + d;					/* fmadds */
}

#ifndef SIZE
#define SIZE 1024
#endif

float vfa[SIZE] __attribute__((__aligned__(32)));
float vfb[SIZE] __attribute__((__aligned__(32)));
float vfc[SIZE] __attribute__((__aligned__(32)));
float vfd[SIZE] __attribute__((__aligned__(32)));

void
vector_fmaf (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    vfa[i] = __builtin_fmaf (vfb[i], vfc[i], vfd[i]);	/* vaddfp */
}

void
vnormal_fmaf (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    vfa[i] = (vfb[i] * vfc[i]) + vfd[i];		/* vaddfp */
}
