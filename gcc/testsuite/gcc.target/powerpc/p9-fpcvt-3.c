/* { dg-do compile { target lp64 } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

/* Note that for unsigned cases, the differences from those ones in
   p9-fpcvt-2.c is that they will be converted to int implicitly first
   and then to floating point.  */

double sc_df (signed char    *p, double df) { return *p + df; }
double uc_df (unsigned char  *p, double df) { return *p + df; }
double ss_df (signed short   *p, double df) { return *p + df; }
double us_df (unsigned short *p, double df) { return *p + df; }

float sc_sf (signed char    *p, float sf) { return *p + sf; }
float uc_sf (unsigned char  *p, float sf) { return *p + sf; }
float ss_sf (signed short   *p, float sf) { return *p + sf; }
float us_sf (unsigned short *p, float sf) { return *p + sf; }

/* { dg-final { scan-assembler     {\mlxsibzx\M}  } } */
/* { dg-final { scan-assembler     {\mlxsihzx\M}  } } */
/* { dg-final { scan-assembler     {\mvextsb2d\M} } } */
/* { dg-final { scan-assembler     {\mvextsh2d\M} } } */
/* { dg-final { scan-assembler-not {\mm[tf]vsr}   } } */
