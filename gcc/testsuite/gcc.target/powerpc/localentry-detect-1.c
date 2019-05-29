/* { dg-do compile } */
/* { dg-require-effective-target powerpc_elfv2 } */
/* { dg-options "-O2 -mdejagnu-cpu=future" } */


int localentry1 () { return 5; }

#pragma GCC target ("cpu=power9")
int localentry2 () { return 5; }

/* { dg-final { scan-assembler {\.localentry\tlocalentry1,1\M} } } */
/* { dg-final { scan-assembler-not {\.localentry\tlocalentry2,1\M} } } */
