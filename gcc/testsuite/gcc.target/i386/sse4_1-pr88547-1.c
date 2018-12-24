/* PR target/88547 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1 -mno-sse4.2" } */
/* { dg-final { scan-assembler-not "pmingt\[bwd]\[\t ]" } } */
/* { dg-final { scan-assembler-times "pminub\[\t ]" 2 } } */
/* { dg-final { scan-assembler-times "pminsb\[\t ]" 2 } } */
/* { dg-final { scan-assembler-times "pminuw\[\t ]" 2 } } */
/* { dg-final { scan-assembler-times "pminsw\[\t ]" 2 } } */
/* { dg-final { scan-assembler-times "pminud\[\t ]" 2 } } */
/* { dg-final { scan-assembler-times "pminsd\[\t ]" 2 } } */

#include "sse2-pr88547-1.c"
