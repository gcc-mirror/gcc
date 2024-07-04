/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mapx-features=egpr,push2pop2,ppx" } */

#include "apx-push2pop2-1.c"

/* { dg-final { scan-assembler "pushp" } } */
/* { dg-final { scan-assembler "popp" } } */
/* { dg-final { scan-assembler "push2p" } } */
/* { dg-final { scan-assembler "pop2p" } } */
