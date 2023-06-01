/* { dg-do compile { target powerpc*-*-darwin* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-Wno-long-long" } */

#pragma pack(push, 1)

#include "darwin-structs-0.h"

int tcd[sizeof(cd) != 9 ? -1 : 1];
int acd[__alignof__(cd) != 1 ? -1 : 1];

int sdc[sizeof(dc) != 9 ? -1 : 1];
int adc[__alignof__(dc) != 1 ? -1 : 1];

int scL[sizeof(cL) != 9 ? -1 : 1];
int acL[__alignof__(cL) != 1 ? -1 : 1];

int sLc[sizeof(Lc) != 9 ? -1 : 1];
int aLc[__alignof__(Lc) != 1 ? -1 : 1];

int scD[sizeof(cD) != 17 ? -1 : 1];
int acD[__alignof__(cD) != 1 ? -1 : 1];

int sDc[sizeof(Dc) != 17 ? -1 : 1];
int aDc[__alignof__(Dc) != 1 ? -1 : 1];

#pragma pack(pop)
