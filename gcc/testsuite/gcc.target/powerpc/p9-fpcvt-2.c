/* { dg-do compile { target { powerpc64*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

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
