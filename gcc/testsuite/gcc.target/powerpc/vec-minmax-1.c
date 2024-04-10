/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power9 -mvsx" } */
/* { dg-final { scan-assembler-times {\mxvmaxdp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvmaxsp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvmindp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvminsp\M} 1 } } */

/* This test verifies that float or double vec_min/max are bound to
   xv[min|max][d|s]p instructions when fast-math is not set.  */


#include <altivec.h>

#ifdef _BIG_ENDIAN
   const int PREF_D = 0;
#else
   const int PREF_D = 1;
#endif

double vmaxd (double a, double b)
{
  vector double va = vec_promote (a, PREF_D);
  vector double vb = vec_promote (b, PREF_D);
  return vec_extract (vec_max (va, vb), PREF_D);
}

double vmind (double a, double b)
{
  vector double va = vec_promote (a, PREF_D);
  vector double vb = vec_promote (b, PREF_D);
  return vec_extract (vec_min (va, vb), PREF_D);
}

#ifdef _BIG_ENDIAN
   const int PREF_F = 0;
#else
   const int PREF_F = 3;
#endif

float vmaxf (float a, float b)
{
  vector float va = vec_promote (a, PREF_F);
  vector float vb = vec_promote (b, PREF_F);
  return vec_extract (vec_max (va, vb), PREF_F);
}

float vminf (float a, float b)
{
  vector float va = vec_promote (a, PREF_F);
  vector float vb = vec_promote (b, PREF_F);
  return vec_extract (vec_min (va, vb), PREF_F);
}
