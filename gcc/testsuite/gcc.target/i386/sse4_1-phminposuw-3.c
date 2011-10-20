/* { dg-do compile } */
/* { dg-options "-O3 -msse4.1 -mno-avx2" } */

#include "sse4_1-phminposuw-2.c"

/* { dg-final { scan-assembler "phminposuw\[^\n\r\]*xmm" } } */
