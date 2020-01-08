/* PR target/88547 */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-xop -mavx512vl -mno-avx512bw -mno-avx512dq" } */
/* { dg-final { scan-assembler-not "vpmingt\[bwdq]\[\t ]" } } */
/* { dg-final { scan-assembler-times "vpminub\[\t ]" 2 } } */
/* { dg-final { scan-assembler-times "vpminsb\[\t ]" 2 } } */
/* { dg-final { scan-assembler-times "vpminuw\[\t ]" 2 } } */
/* { dg-final { scan-assembler-times "vpminsw\[\t ]" 2 } } */
/* { dg-final { scan-assembler-times "vpcmp\[dq\]\[\t ]" 4 } } */
/* { dg-final { scan-assembler-times "vpcmpu\[dq\]\[\t ]" 4 } } */
/* { dg-final { scan-assembler-times "vpternlog\[qd\]\[\t ]" 8 } } */
#include "avx2-pr88547-1.c"
