/* { dg-do compile } */
/* { dg-options "-O2 -mstrict-align" } */

#define TEST_TARGET_PRAGMA 1
#include "./strict_align-func-attr-1.c"

/* { dg-final { scan-assembler-not "ld.bu" } } */
