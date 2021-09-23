/* PR target/100648 */
/* { dg-do compile } */
/* { dg-options "-O2 -std=c++14 -mavx512vl -mavx512bw -mno-xop" } */
/* { dg-final { scan-assembler-not "pcmpgt\[bdq]" } } */
/* { dg-final { scan-assembler-times "pblendvb" 2 } } */
/* { dg-final { scan-assembler-times "blendvps" 4 } } */
/* { dg-final { scan-assembler-times "blendvpd" 4 } } */

#include "sse4_1-pr54700-1.C"
