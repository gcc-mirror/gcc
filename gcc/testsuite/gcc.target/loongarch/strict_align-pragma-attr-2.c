/* { dg-do compile } */
/* { dg-options "-O2 -mno-strict-align" } */

#define TEST_TARGET_PRAGMA 1
#include "./strict_align-func-attr-2.c"

/* { dg-final { scan-assembler-not "ld.w" } } */
