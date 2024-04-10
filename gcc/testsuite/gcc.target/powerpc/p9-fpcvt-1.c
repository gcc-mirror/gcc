/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */

void sc (signed char    *p, double x) { *p = x; }
void uc (unsigned char  *p, double x) { *p = x; }
void ss (signed short   *p, double x) { *p = x; }
void us (unsigned short *p, double x) { *p = x; }

/* { dg-final { scan-assembler     "stxsibx" } } */
/* { dg-final { scan-assembler     "stxsihx" } } */
/* { dg-final { scan-assembler-not "mfvsrd"  } } */
/* { dg-final { scan-assembler-not "mfvsrwz" } } */
/* { dg-final { scan-assembler-not "mtvsrd"  } } */
/* { dg-final { scan-assembler-not "mtvsrwa" } } */
/* { dg-final { scan-assembler-not "mtvsrwz" } } */
