// PR117013
/* { dg-do run } */
// { dg-options "-O2 -std=c++20 -save-temps" }

#define fp_type double
#include "spaceship_2.C"

/* { dg-final { scan-assembler-not "\tfcmp\t" } } */
/* { dg-final { scan-assembler-times "\tfcmpe\t" 2 } } */