/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */

long long foo (long long c) { return c >= 0 ? 0x400000000ll : -1ll; }

/* { dg-final { scan-assembler "bts" } } */
