/* { dg-do compile } */
/* { dg-additional-options "-march=gfx908 -O1" } */
/* { dg-final { scan-assembler "accvgpr" } } */

typedef long v32di __attribute__ ((__vector_size__ (sizeof(long)*32)));
#define TYPE v32di

#include "avgpr-spill-int.c"
