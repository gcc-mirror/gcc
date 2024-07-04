/* PR target/83862.c */
/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-options "-mvsx -O2 -mabi=ieeelongdouble -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target ppc_float128_sw } */
/* { dg-require-effective-target powerpc_vsx } */

/* On little endian systems, optimizing signbit of IEEE 128-bit values from
   memory could abort if the memory address was indexed (reg+reg).  The
   optimization is only on 64-bit machines with direct move.

   Compile with -g -O2 -mabi=ieeelongdouble -Wno-psabi.  */

#ifndef TYPE
#define TYPE long double
#endif

int sbr (TYPE a) { return __builtin_signbit (a); }
int sbm (TYPE *a) { return __builtin_signbit (*a); }
int sbo (TYPE *a) { return __builtin_signbit (a[4]); }
int sbi (TYPE *a, unsigned long n) { return __builtin_signbit (a[n]); }
void sbs (int *p, TYPE a) { *p = __builtin_signbit (a); }

/* On big endian systems, this will generate 2 LDs and 1 LDX, while on
   little endian systems, this will generate 3 LDs and an ADD.  */

/* { dg-final { scan-assembler-times {\mldx?\M}    3 } } */
/* { dg-final { scan-assembler-times {\mmfvsrd\M}  2 } } */
/* { dg-final { scan-assembler-times {\msrdi\M}    5 } } */
/* { dg-final { scan-assembler-not   {\mmfvsrld\M}   } } */
/* { dg-final { scan-assembler-not   {\mstxvx?\M}    } } */
/* { dg-final { scan-assembler-not   {\mstxvw4x\M}   } } */
/* { dg-final { scan-assembler-not   {\mstxvd2x\M}   } } */
/* { dg-final { scan-assembler-not   {\mstvx\M}      } } */

