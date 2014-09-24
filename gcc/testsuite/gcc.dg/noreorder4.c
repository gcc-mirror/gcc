/* { dg-do compile } */
/* { dg-options "-O2 -fno-toplevel-reorder" } */
#define NOREORDER
#include "noreorder.c"
/* { dg-final { scan-assembler "bozo.*jukjuk.*func1.*barbar.*func2" } } */
