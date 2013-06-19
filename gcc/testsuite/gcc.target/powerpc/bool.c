/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "eqv" } } */
/* { dg-final { scan-assembler "nand" } } */
/* { dg-final { scan-assembler "nor" } } */

#ifndef TYPE
#define TYPE unsigned long
#endif

TYPE op1 (TYPE a, TYPE b) { return ~(a ^ b); }	/* eqv */
TYPE op2 (TYPE a, TYPE b) { return ~(a & b); }	/* nand */
TYPE op3 (TYPE a, TYPE b) { return ~(a | b); }	/* nor */

