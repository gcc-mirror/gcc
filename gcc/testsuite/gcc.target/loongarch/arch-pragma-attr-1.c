/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mno-lsx -std=gnu11" } */

#define TEST_TARGET_PRAGMA 1
#include "./arch-func-attr-1.c"

/* { dg-final { scan-assembler "vld" } } */
