/* PR target/88808  */
/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512dq -O2" } */
/* { dg-final { scan-assembler-times "korb" "1" } }  */
/* { dg-final { scan-assembler-times "kxorb" "1" } }  */
#include "bitwise_mask_op-1.c"

