/* { dg-do compile } */
/* { dg-additional-options "-march=gfx90a -O1" } */
/* { dg-final { scan-assembler {load[^\n]*a[0-9[]} } } */
/* { dg-final { scan-assembler {store[^\n]*a[0-9[]} } } */

#define TYPE short

#include "avgpr-mem-int.c"
