/* { dg-do compile } */
/* { dg-options "-O2 -mno-lsx" } */

#define TEST_TARGET_PRAGMA 1
#include "./lsx-func-attr-1.c"

/* { dg-final { scan-assembler "vadd.w" } } */
