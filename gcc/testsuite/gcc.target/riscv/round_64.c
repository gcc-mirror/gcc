/* { dg-do compile { target { riscv64*-*-* } } } */
/* { dg-require-effective-target glibc } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fno-math-errno -funsafe-math-optimizations -fno-inline" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

#include "round.c"

/* { dg-final { scan-assembler-times {\mfcvt.w.s} 10 } } */
/* { dg-final { scan-assembler-times {\mfcvt.s.w} 5 } } */
/* { dg-final { scan-assembler-times {\mfcvt.l.d} 10 } } */
/* { dg-final { scan-assembler-times {\mfcvt.d.l} 45 } } */
/* { dg-final { scan-assembler-times {\mfcvt.w.d} 5 } } */
/* { dg-final { scan-assembler-times {,rup} 6 } } */
/* { dg-final { scan-assembler-times {,rmm} 6 } } */
/* { dg-final { scan-assembler-times {,rdn} 6 } } */
/* { dg-final { scan-assembler-times {,rtz} 6 } } */
/* { dg-final { scan-assembler-not "\\sceil\\s" } } */
/* { dg-final { scan-assembler-not "\\sfloor\\s" } } */
/* { dg-final { scan-assembler-not "\\sround\\s" } } */
/* { dg-final { scan-assembler-not "\\snearbyint\\s" } } */
/* { dg-final { scan-assembler-not "\\srint\\s" } } */
/* { dg-final { scan-assembler-not "\\stail\\s" } } */
/* { dg-final { scan-assembler-not "\\ssext.w\\s" } } */

