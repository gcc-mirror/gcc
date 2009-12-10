/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-aix* } { "*" } { "" } } */
/* { dg-options "-O2 -mpowerpc64" } */
/* { dg-require-effective-target ilp32 } */
/* { dg-final { scan-assembler-times "lwbrx" 2 } } */
/* { dg-final { scan-assembler-times "stwbrx" 2 } } */

long long swap_load (long long *a) { return __builtin_bswap64 (*a); }
long long swap_reg (long long a) { return __builtin_bswap64 (a); }
void swap_store (long long *a, long long b) { *a = __builtin_bswap64 (b); }
