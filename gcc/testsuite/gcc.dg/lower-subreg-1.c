/* { dg-do compile { target { ! { mips64 || { aarch64*-*-* arm*-*-* i?86-*-* ia64-*-* pru-*-* sparc*-*-* x86_64-*-* } } } } } */
/* { dg-options "-O -fdump-rtl-subreg1" } */
/* { dg-require-effective-target ilp32 } */

long long test (long long a, long long b) { return a | b; }

/* { dg-final { scan-rtl-dump "Splitting reg" "subreg1" } } */
