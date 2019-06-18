/* { dg-do compile } */
/* { dg-require-effective-target powerpc_elfv2 } */
/* { dg-require-effective-target powerpc_future_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=future" } */

/* At present, -mcpu=future does not enable pc-relative mode.  Enable it here
   explicitly until it is turned on by default.  */
#pragma GCC target ("cpu=future,pcrel")
int localentry1 () { return 5; }

#pragma GCC target ("cpu=power9,no-pcrel")
int localentry2 () { return 5; }

/* { dg-final { scan-assembler {\.localentry\tlocalentry1,1\M} } } */
/* { dg-final { scan-assembler-not {\.localentry\tlocalentry2,1\M} } } */
