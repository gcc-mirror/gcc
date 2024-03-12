/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9modulo_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

int ismod (int a, int b) { return a%b; }
long lsmod (long a, long b) { return a%b; }
unsigned int iumod (unsigned int a, unsigned int b) { return a%b; }
unsigned long lumod (unsigned long a, unsigned long b) { return a%b; }

/* { Fail due to RS6000_DISABLE_SCALAR_MODULO. */
/* { dg-final { scan-assembler-times {\mmodsw\M} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\mmodsd\M} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\mmoduw\M} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\mmodud\M} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-not   {\mmullw\M}   { xfail *-*-* } } } */
/* { dg-final { scan-assembler-not   {\mmulld\M}   { xfail *-*-* } } } */
/* { dg-final { scan-assembler-not   {\mdivw\M}    { xfail *-*-* } } } */
/* { dg-final { scan-assembler-not   {\mdivd\M}    { xfail *-*-* } } } */
/* { dg-final { scan-assembler-not   {\mdivwu\M}   { xfail *-*-* } } } */
/* { dg-final { scan-assembler-not   {\mdivdu\M}   { xfail *-*-* } } } */
