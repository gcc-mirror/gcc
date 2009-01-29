/* { dg-do compile { target { { { ! mips64 } && { ! ia64-*-* } } && { ! spu-*-* } } } } */
/* { dg-options "-O -fdump-rtl-subreg1" } */
/* { dg-require-effective-target ilp32 } */

long long test (long long a, long long b) { return a | b; }

/* { dg-final { scan-rtl-dump "Splitting reg" "subreg1" } } */
/* { dg-final { cleanup-rtl-dump "subreg1" } } */
