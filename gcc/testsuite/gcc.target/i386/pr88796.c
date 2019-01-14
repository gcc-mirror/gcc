/* PR rtl-optimization/88796 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -fstack-protector-strong" } */
/* { dg-require-effective-target fstack_protector } */

#include "pr87370.c"

/* { dg-final { scan-assembler-not "xmm" } } */
