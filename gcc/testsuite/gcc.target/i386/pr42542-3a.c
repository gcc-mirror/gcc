/* { dg-do compile } */
/* { dg-options "-O1 -msse2 -ftree-vectorize" } */
/* { dg-require-effective-target sse2 } */

#include "pr42542-3.c"

/* { dg-final { scan-assembler "pmaxub" } } */
/* { dg-final { scan-assembler "pminub" } } */
