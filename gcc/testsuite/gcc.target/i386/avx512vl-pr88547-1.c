/* PR target/88547 */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-xop -mavx512vl -mavx512bw -mavx512dq" } */
/* { dg-final { scan-assembler-not "vpmingt\[bwdq]\[\t ]" } } */
/* { dg-final { scan-assembler-not "%k\[0-9\]" } } */
/* { dg-final { scan-assembler-times "vpminub\[\t ]" 2 } } */
/* { dg-final { scan-assembler-times "vpminsb\[\t ]" 2 } } */
/* { dg-final { scan-assembler-times "vpminuw\[\t ]" 2 } } */
/* { dg-final { scan-assembler-times "vpminsw\[\t ]" 2 } } */
/* { dg-final { scan-assembler-times "vpminud\[\t ]" 2 } } */
/* { dg-final { scan-assembler-times "vpminsd\[\t ]" 2 } } */
/* { dg-final { scan-assembler-times "vpminuq\[\t ]" 2 } } */
/* { dg-final { scan-assembler-times "vpminsq\[\t ]" 2 } } */
#include "avx2-pr88547-1.c"
