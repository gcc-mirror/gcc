/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power9 -mvsx -ffast-math" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-final { scan-assembler-times {\mxsmaxcdp\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxsmincdp\M} 2 } } */

/* This test verifies that float or double vec_min/max can be converted
   to scalar comparison when fast-math is set.  */


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
