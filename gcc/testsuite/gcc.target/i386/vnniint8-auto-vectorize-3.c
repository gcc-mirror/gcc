/* { dg-do compile } */                                     
/* { dg-options "-mavx10.2 -O2" } */
/* { dg-final { scan-assembler "vpdpbssd\t" } } */
/* { dg-final { scan-assembler "vpdpbuud\t" } } */

#include "vnniint8-auto-vectorize-1.c"
