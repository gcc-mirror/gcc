/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */
/* { dg-require-effective-target power10_ok } */

unsigned long long msk66() { return 0x6666666666666666ULL; }

/* { dg-final { scan-assembler-times {\mpli\M} 1 } } */
/* { dg-final { scan-assembler-not {\mli\M} } } */
/* { dg-final { scan-assembler-not {\mlis\M} } } */
