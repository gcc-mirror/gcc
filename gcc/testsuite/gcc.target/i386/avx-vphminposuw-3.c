/* { dg-do compile } */
/* { dg-options "-O3 -mavx -mno-avx2" } */

#include "avx-vphminposuw-2.c"

/* { dg-final { scan-assembler "vphminposuw\[^\n\r\]*xmm" } } */
