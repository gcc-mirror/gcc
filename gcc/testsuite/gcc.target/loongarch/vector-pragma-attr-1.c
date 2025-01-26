/* { dg-do compile } */
/* { dg-options "-O2 -mlsx" } */

#define TEST_TARGET_PRAGMA 1
#include "./vector-func-attr-1.c"

/* { dg-final { scan-assembler "vadd.w" } } */
