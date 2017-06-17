/* { dg-do compile { target { ! { mips64 || { aarch64*-*-* arm*-*-* ia64-*-* sparc*-*-* spu-*-* tilegx-*-* } } } } } */
/* { dg-options "-O -fdump-rtl-subreg1" } */
/* { dg-additional-options "-mno-stv" { target ia32 } } */
/* { dg-skip-if "" { { i?86-*-* x86_64-*-* } && x32 } } */
/* { dg-require-effective-target ilp32 } */

long long test (long long a, long long b) { return a | b; }

/* { dg-final { scan-rtl-dump "Splitting reg" "subreg1" } } */
