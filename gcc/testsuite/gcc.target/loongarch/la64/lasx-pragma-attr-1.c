/* { dg-do compile } */
/* { dg-options "-O2 -mno-lsx" } */

#define TEST_TARGET_PRAGMA 1
#include "./lasx-func-attr-1.c"

/* { dg-final { scan-assembler "xvadd.w" } } */
