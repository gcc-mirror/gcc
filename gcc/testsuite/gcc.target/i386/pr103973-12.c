/* PR target/103973 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -march=i686 -mfpmath=387" } */
/* { dg-final { scan-assembler-not "'\tfucom" } } */
/* { dg-final { scan-assembler-times "\tfcom" 4 } } */

#define double float
#include "pr103973-9.c"
