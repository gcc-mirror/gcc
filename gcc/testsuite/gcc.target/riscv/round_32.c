/* { dg-do compile { target { riscv32*-*-* } } } */
/* { dg-require-effective-target glibc } */
/* { dg-options "-march=rv32gc -mabi=ilp32d -fno-math-errno -funsafe-math-optimizations -fno-inline" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

#include "round.c"

/* { dg-final { scan-assembler-times {\mfcvt.w.s} 15 } } */
/* { dg-final { scan-assembler-times {\mfcvt.s.w} 5 } } */
/* { dg-final { scan-assembler-times {\mfcvt.d.w} 65 } } */
/* { dg-final { scan-assembler-times {\mfcvt.w.d} 15 } } */
/* { dg-final { scan-assembler-times {,rup} 6 } } */
/* { dg-final { scan-assembler-times {,rmm} 6 } } */
/* { dg-final { scan-assembler-times {,rdn} 6 } } */
/* { dg-final { scan-assembler-times {,rtz} 6 } } */
/* { dg-final { scan-assembler-not {\mfcvt.l.d} } } */
/* { dg-final { scan-assembler-not {\mfcvt.d.l} } } */
/* { dg-final { scan-assembler-not "\\sceil\\s" } } */
/* { dg-final { scan-assembler-not "\\sfloor\\s" } } */
/* { dg-final { scan-assembler-not "\\sround\\s" } } */
/* { dg-final { scan-assembler-not "\\snearbyint\\s" } } */
/* { dg-final { scan-assembler-not "\\srint\\s" } } */
/* { dg-final { scan-assembler-not "\\stail\\s" } } */
