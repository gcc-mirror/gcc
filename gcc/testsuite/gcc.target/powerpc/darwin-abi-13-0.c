/* { dg-do compile { target powerpc*-*-darwin* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-Wno-long-long" } */

#include "darwin-structs-0.h"

int tcd[sizeof(cd) != 12 ? -1 : 1];
int acd[__alignof__(cd) != 4 ? -1 : 1];

int sdc[sizeof(dc) != 16 ? -1 : 1];
int adc[__alignof__(dc) != 8 ? -1 : 1];

int scL[sizeof(cL) != 12 ? -1 : 1];
int acL[__alignof__(cL) != 4 ? -1 : 1];

int sLc[sizeof(Lc) != 16 ? -1 : 1];
int aLc[__alignof__(Lc) != 8 ? -1 : 1];

int scD[sizeof(cD) != 32 ? -1 : 1];
int acD[__alignof__(cD) != 16 ? -1 : 1];

int sDc[sizeof(Dc) != 32 ? -1 : 1];
int aDc[__alignof__(Dc) != 16 ? -1 : 1];
