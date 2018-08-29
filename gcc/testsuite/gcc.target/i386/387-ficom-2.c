/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-skip-if "" { *-*-* } { "-march=*" } { "-march=i386" } } */
/* { dg-options "-Os -march=i386 -ffast-math -masm=att" } */

#include "387-ficom-1.c"

/* { dg-final { scan-assembler-times "ficomp\[s\t\]" 3 } } */
/* { dg-final { scan-assembler-times "ficompl" 3 } } */
