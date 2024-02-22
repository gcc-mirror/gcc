/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2" } */

#include <altivec.h>

double
add_double (vector double a, int n)
{
  return vec_extract (a, n) + 1.0;
}

long
add_long (vector long a, int n)
{
  return vec_extract (a, n) + 1;
}

/* { dg-final { scan-assembler     "vslo"    } } */
/* { dg-final { scan-assembler     "mtvsrd"  } } */
/* { dg-final { scan-assembler     "mfvsrd"  } } */
/* { dg-final { scan-assembler-not "stxvd2x" } } */
/* { dg-final { scan-assembler-not "stxvx"   } } */
/* { dg-final { scan-assembler-not "stxv"    } } */
/* { dg-final { scan-assembler-not "ldx"     } } */
