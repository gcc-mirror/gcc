/* { dg-do compile } */                                     
/* { dg-options "-mavx10.2-256 -O2" } */
/* { dg-final { scan-assembler "vpdpwusd\t" } } */
/* { dg-final { scan-assembler "vpdpwuud\t" } } */

#include "vnniint16-auto-vectorize-1.c"
