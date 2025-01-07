/* { dg-do compile } */
/* { dg-options "-O2 -mcmodel=normal -mexplicit-relocs=none" } */

#define TEST_TARGET_PRAGMA 1
#include "./cmodel-func-attr-1.c"

/* { dg-final { scan-assembler "la.global\t\\\$r\[0-9\]+,\\\$r\[0-9\]+,a" } } */
