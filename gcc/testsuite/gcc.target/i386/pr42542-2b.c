/* { dg-do compile } */
/* { dg-options "-O1 -msse4.1 -ftree-vectorize" } */

#define CHECK_H "sse4_1-check.h"
#define TEST sse4_1_test

#include "pr42542-2.c"

/* { dg-final { scan-assembler "pmaxuw" } } */
/* { dg-final { scan-assembler "pminuw" } } */
