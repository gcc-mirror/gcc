#define N 8

#include "./lsx-andn-iorn.c"

/* { dg-do compile } */
/* { dg-options "-O2 -mlasx -ftree-vectorize -fdump-tree-optimized" } */

/* We should produce a BIT_ANDC and BIT_IORC here.  */

/* { dg-final { scan-tree-dump ".BIT_ANDN " "optimized" } } */
/* { dg-final { scan-tree-dump ".BIT_IORN " "optimized" } } */
