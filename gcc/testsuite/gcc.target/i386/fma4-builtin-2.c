/* { dg-do compile } */
/* { dg-options "-O3 -mfma4 -mtune=generic" } */

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
    vda[i] = __builtin_fma (vdb[i], vdc[i], vdd[i]);
}

void
vector_fms (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    vda[i] = __builtin_fma (vdb[i], vdc[i], -vdd[i]);
}

void
vector_fnma (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    vda[i] = __builtin_fma (-vdb[i], vdc[i], vdd[i]);
}

void
vector_fnms (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    vda[i] = __builtin_fma (-vdb[i], vdc[i], -vdd[i]);
}

void
vector_fmaf (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    vfa[i] = __builtin_fmaf (vfb[i], vfc[i], vfd[i]);
}

void
vector_fmsf (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    vfa[i] = __builtin_fmaf (vfb[i], vfc[i], -vfd[i]);
}

void
vector_fnmaf (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    vfa[i] = __builtin_fmaf (-vfb[i], vfc[i], vfd[i]);
}

void
vector_fnmsf (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    vfa[i] = __builtin_fmaf (-vfb[i], vfc[i], -vfd[i]);
}

/* { dg-final { scan-assembler-times "vfmaddps" 1 } } */
/* { dg-final { scan-assembler-times "vfmaddpd" 1 } } */
/* { dg-final { scan-assembler-times "vfmsubps" 1 } } */
/* { dg-final { scan-assembler-times "vfmsubpd" 1 } } */
/* { dg-final { scan-assembler-times "vfnmaddps" 1 } } */
/* { dg-final { scan-assembler-times "vfnmaddpd" 1 } } */
/* { dg-final { scan-assembler-times "vfnmsubps" 1 } } */
/* { dg-final { scan-assembler-times "vfnmsubpd" 1 } } */
