/* { dg-do compile } */
/* { dg-options "-mgfni -O3 -Wno-shift-count-negative" } */
/* { dg-final { scan-assembler-times "vgf2p8affineqb" 0 } } */

#include "shift-gf2p8affine-2.c"
