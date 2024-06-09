/* { dg-do compile { target lp64 } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

double sc (signed char    *p) { return (double)*p; }
double uc (unsigned char  *p) { return (double)*p; }
double ss (signed short   *p) { return (double)*p; }
double us (unsigned short *p) { return (double)*p; }

/* { dg-final { scan-assembler     "lxsibzx"  } } */
/* { dg-final { scan-assembler     "lxsihzx"  } } */
/* { dg-final { scan-assembler     "vextsb2d" } } */
/* { dg-final { scan-assembler     "vextsh2d" } } */
/* { dg-final { scan-assembler-not "mfvsrd"   } } */
/* { dg-final { scan-assembler-not "mfvsrwz"  } } */
/* { dg-final { scan-assembler-not "mtvsrd"   } } */
/* { dg-final { scan-assembler-not "mtvsrwa"  } } */
/* { dg-final { scan-assembler-not "mtvsrwz"  } } */
