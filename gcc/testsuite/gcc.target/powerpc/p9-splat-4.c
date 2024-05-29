/* { dg-do compile { target lp64 } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector long long foo (long long a) { return (vector long long) { a, a }; }

/* { dg-final { scan-assembler "mtvsrdd" } } */
