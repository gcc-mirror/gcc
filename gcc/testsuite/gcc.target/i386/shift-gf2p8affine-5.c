/* { dg-do compile } */
/* { dg-options "-mgfni -mavx2 -O3 -Wno-shift-count-negative -march=x86-64 -mtune=generic" } */
/* { dg-final { scan-assembler-times "vgf2p8affineqb" 46 } } */

#include "shift-gf2p8affine-2.c"
