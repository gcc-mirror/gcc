/* PR target/100648 */
/* { dg-do compile } */
/* { dg-options "-O2 -std=c++14 -mavx2 -mno-xop -mavx512vl -mavx512bw" } */
/* { dg-final { scan-assembler-not "vpcmpgt\[bdq]" } } */
/* { dg-final { scan-assembler-times "vpblendvb" 2 } } */
/* { dg-final { scan-assembler-times "vblendvps" 4 } } */
/* { dg-final { scan-assembler-times "vblendvpd" 4 } } */
/* { dg-skip-if "requires hosted libstdc++ for cstdlib malloc" { ! hostedlib } } */

#include "avx2-pr54700-1.C"
