/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O2 -mdejagnu-cpu=power7" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-final { scan-assembler "xxpermdi" } } */
/* { dg-final { scan-assembler-not "stxvd2x" } } */

/* Make sure double extract doesn't use a store instruction.  */

double d0(__vector double v){ return __builtin_vec_extract (v, 0); }
double d1(__vector double v){ return __builtin_vec_extract (v, 1); }

double e0(vector double v){ return __builtin_vec_ext_v2df (v, 0); }
double e1(vector double v){ return __builtin_vec_ext_v2df (v, 1); }
