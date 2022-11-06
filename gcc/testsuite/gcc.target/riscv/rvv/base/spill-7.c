/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -mpreferred-stack-boundary=3 -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"
#include "macro.h"

/*
** spill:
**  csrr\tt0,vlenb
**  slli\tt1,t0,4
**  sub\tsp,sp,t1
**  vsetvli\ta3,zero,e8,mf8,ta,ma
**  vle8.v\tv24,0\(a0\)
**  csrr\ta5,vlenb
**  srli\ta5,a5,3
**  add\ta5,a5,sp
**  vse8.v\tv24,0\(a5\)
**  addi\ta5,a0,1
**  vsetvli\ta4,zero,e8,mf4,ta,ma
**  vle8.v\tv24,0\(a5\)
**  csrr\ta5,vlenb
**  srli\ta5,a5,2
**  add\ta5,a5,sp
**  vse8.v\tv24,0\(a5\)
**  addi\ta2,a0,2
**  vsetvli\ta5,zero,e8,mf2,ta,ma
**  vle8.v\tv24,0\(a2\)
**  csrr\ta2,vlenb
**  srli\ta2,a2,1
**  add\ta2,a2,sp
**  vse8.v\tv24,0\(a2\)
**  addi\ta2,a0,3
**  vl1re8.v\tv24,0\(a2\)
**  csrr\ta2,vlenb
**  add\ta2,a2,sp
**  vs1r.v\tv24,0\(a2\)
**  addi\ta2,a0,4
**  vl2re8.v\tv24,0\(a2\)
**  csrr\tt3,vlenb
**  slli\ta2,t3,1
**  add\ta2,a2,sp
**  vs2r.v\tv24,0\(a2\)
**  addi\ta2,a0,5
**  vl4re8.v\tv24,0\(a2\)
**  mv\ta2,t3
**  slli\tt3,t3,2
**  add\tt3,t3,sp
**  vs4r.v\tv24,0\(t3\)
**  addi\ta0,a0,6
**  vl8re8.v\tv24,0\(a0\)
**  slli\ta0,a2,3
**  add\ta0,a0,sp
**  vs8r.v\tv24,0\(a0\)
**  ...
**  srli\ta0,a2,3
**  add\ta0,a0,sp
**  ...
**  vle8.v\tv27,0\(a0\)
**  vse8.v\tv27,0\(a1\)
**  addi\ta3,a1,1
**  srli\ta0,a2,2
**  add\ta0,a0,sp
**  ...
**  vle8.v\tv27,0\(a0\)
**  vse8.v\tv27,0\(a3\)
**  addi\ta4,a1,2
**  srli\ta3,a2,1
**  add\ta3,a3,sp
**  ...
**  vle8.v\tv27,0\(a3\)
**  vse8.v\tv27,0\(a4\)
**  addi\ta5,a1,3
**  add\ta4,a2,sp
**  vl1re8.v\tv25,0\(a4\)
**  vs1r.v\tv25,0\(a5\)
**  addi\ta5,a1,4
**  slli\ta4,a2,1
**  add\ta4,a4,sp
**  vl2re8.v\tv26,0\(a4\)
**  vs2r.v\tv26,0\(a5\)
**  addi\ta5,a1,5
**  vl4re8.v\tv28,0\(t3\)
**  vs4r.v\tv28,0\(a5\)
**  addi\ta1,a1,6
**  slli\ta5,a2,3
**  add\ta5,a5,sp
**  vl8re8.v\tv24,0\(a5\)
**  vs8r.v\tv24,0\(a1\)
**  csrr\tt0,vlenb
**  slli\tt1,t0,4
**  add\tsp,sp,t1
**  ...
**  jr\tra
*/
void
spill (int8_t *in, int8_t *out)
{
  vint8mf8_t v0 = *(vint8mf8_t*)in;
  vint8mf4_t v1 = *(vint8mf4_t*)(in + 1);
  vint8mf2_t v2 = *(vint8mf2_t*)(in + 2);
  vint8m1_t v3 = *(vint8m1_t*)(in + 3);
  vint8m2_t v4 = *(vint8m2_t*)(in + 4);
  vint8m4_t v8 = *(vint8m4_t*)(in + 5);
  vint8m8_t v16 = *(vint8m8_t*)(in + 6);
  exhaust_vector_regs (); 
  *(vint8mf8_t*)out = v0;
  *(vint8mf4_t*)(out + 1) = v1; 
  *(vint8mf2_t*)(out + 2) = v2;
  *(vint8m1_t*)(out + 3) = v3;
  *(vint8m2_t*)(out + 4) = v4;
  *(vint8m4_t*)(out + 5) = v8;
  *(vint8m8_t*)(out + 6) = v16;
}
