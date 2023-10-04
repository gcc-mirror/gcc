/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target glibc } */

#include "../../../../gcc.dg/pr90263.c"

/* { dg-final { scan-assembler-not "memcpy" { target { riscv_v } } } } */
