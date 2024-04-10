/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */

/* Verify P9 vector negate instructions.  */

vector long long v2di_neg (vector long long a) { return -a; }
vector int v4si_neg (vector int a) { return -a; }

/* { dg-final { scan-assembler "vnegd" } } */
/* { dg-final { scan-assembler "vnegw" } } */
