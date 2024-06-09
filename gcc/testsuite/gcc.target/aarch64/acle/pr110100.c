/* { dg-do compile } */
/* { dg-options "-march=armv8.7-a+ls64 -O2" } */
#include <arm_acle.h>
void do_st64b(data512_t data) {
  __arm_st64b((void*)0x10000000, data);
}
/* { dg-final { scan-assembler {mov\tx([123])?[0-9], 268435456} } } */
