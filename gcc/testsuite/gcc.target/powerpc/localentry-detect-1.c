/* { dg-do compile } */
/* { dg-require-effective-target powerpc_elfv2 } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* At present, -mcpu=power10 does not enable pc-relative mode.  Enable it here
   explicitly until it is turned on by default.  */
#pragma GCC target ("cpu=power10,pcrel")
int localentry1 () { return 5; }

#pragma GCC target ("cpu=power9,no-pcrel")
int localentry2 () { return 5; }

/* { dg-final { scan-assembler {\.localentry\tlocalentry1,1\M} } } */
/* { dg-final { scan-assembler-not {\.localentry\tlocalentry2,1\M} } } */
