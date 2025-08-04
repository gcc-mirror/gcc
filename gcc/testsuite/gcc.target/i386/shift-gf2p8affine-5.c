/* { dg-do compile } */
/* { dg-options "-mgfni -mavx -O3 -Wno-shift-count-negative" } */
/* { dg-final { scan-assembler-times "vgf2p8affineqb" 31 } } */

#include "shift-gf2p8affine-2.c"
