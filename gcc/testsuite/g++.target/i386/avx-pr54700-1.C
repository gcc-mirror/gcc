/* PR target/54700 */
/* { dg-do compile } */
/* { dg-options "-O2 -std=c++14 -mavx -mno-xop -mno-avx2" } */
/* { dg-final { scan-assembler-not "vpcmpgt\[bdq]" } } */
/* { dg-final { scan-assembler-times "vpblendvb" 2 } } */
/* { dg-final { scan-assembler-times "vblendvps" 4 } } */
/* { dg-final { scan-assembler-times "vblendvpd" 4 } } */

#include "sse4_1-pr54700-1.C"
