/* PR target/103973 */
/* { dg-do run { target large_long_double } } */
/* { dg-options "-O2 -ffast-math -save-temps" } */
/* { dg-final { scan-assembler-not "'\tfucom" } } */
/* { dg-final { scan-assembler-times "\tfcom" 4 } } */

#define double long double
#include "pr103973-13.c"
