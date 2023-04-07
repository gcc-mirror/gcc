/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -mpreferred-stack-boundary=3 -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"
#include "macro.h"

/*
** spill:
**  csrr\t[a-x0-9]+,vlenb
**  slli\t[a-x0-9]+,[a-x0-9]+,4
**  sub\tsp,[a-x0-9]+,[a-x0-9]+
**  vsetvli\t[a-x0-9]+,zero,e8,mf8,ta,ma
**  vle8.v\tv[0-9]+,0\([a-x0-9]+\)
**  csrr\t[a-x0-9]+,vlenb
**  srli\t[a-x0-9]+,[a-x0-9]+,3
**  add\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
**  vse8.v\tv[0-9]+,0\([a-x0-9]+\)
**  addi\t[a-x0-9]+,[a-x0-9]+,1
**  vsetvli\t[a-x0-9]+,zero,e8,mf4,ta,ma
**  vle8.v\tv[0-9]+,0\([a-x0-9]+\)
**  csrr\t[a-x0-9]+,vlenb
**  srli\t[a-x0-9]+,[a-x0-9]+,2
**  add\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
**  vse8.v\tv[0-9]+,0\([a-x0-9]+\)
**  addi\t[a-x0-9]+,[a-x0-9]+,2
**  vsetvli\t[a-x0-9]+,zero,e8,mf2,ta,ma
**  vle8.v\tv[0-9]+,0\([a-x0-9]+\)
**  csrr\t[a-x0-9]+,vlenb
**  srli\t[a-x0-9]+,[a-x0-9]+,1
**  add\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
**  vse8.v\tv[0-9]+,0\([a-x0-9]+\)
**  addi\t[a-x0-9]+,[a-x0-9]+,3
**  vl1re8.v\tv[0-9]+,0\([a-x0-9]+\)
**  csrr\t[a-x0-9]+,vlenb
**  add\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
**  vs1r.v\tv[0-9]+,0\([a-x0-9]+\)
**  addi\t[a-x0-9]+,[a-x0-9]+,4
**  vl2re8.v\tv[0-9]+,0\([a-x0-9]+\)
**  csrr\t[a-x0-9]+,vlenb
**  slli\t[a-x0-9]+,[a-x0-9]+,1
**  add\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
**  vs2r.v\tv[0-9]+,0\([a-x0-9]+\)
**  addi\t[a-x0-9]+,[a-x0-9]+,5
**  vl4re8.v\tv[0-9]+,0\([a-x0-9]+\)
**  mv\t[a-x0-9]+,[a-x0-9]+
**  slli\t[a-x0-9]+,[a-x0-9]+,2
**  add\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
**  vs4r.v\tv[0-9]+,0\([a-x0-9]+\)
**  addi\t[a-x0-9]+,[a-x0-9]+,6
**  vl8re8.v\tv[0-9]+,0\([a-x0-9]+\)
**  ...
**  slli\t[a-x0-9]+,[a-x0-9]+,3
**  add\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
**  vs8r.v\tv[0-9]+,0\([a-x0-9]+\)
**  ...
**  srli\t[a-x0-9]+,[a-x0-9]+,3
**  add\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
**  ...
**  vle8.v\tv[0-9]+,0\([a-x0-9]+\)
**  vse8.v\tv[0-9]+,0\([a-x0-9]+\)
**  addi\t[a-x0-9]+,[a-x0-9]+,1
**  srli\t[a-x0-9]+,[a-x0-9]+,2
**  add\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
**  ...
**  vle8.v\tv[0-9]+,0\([a-x0-9]+\)
**  vse8.v\tv[0-9]+,0\([a-x0-9]+\)
**  addi\t[a-x0-9]+,[a-x0-9]+,2
**  srli\t[a-x0-9]+,[a-x0-9]+,1
**  add\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
**  ...
**  vle8.v\tv[0-9]+,0\([a-x0-9]+\)
**  vse8.v\tv[0-9]+,0\([a-x0-9]+\)
**  addi\t[a-x0-9]+,[a-x0-9]+,3
**  add\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
**  vl1re8.v\tv[0-9]+,0\([a-x0-9]+\)
**  vs1r.v\tv[0-9]+,0\([a-x0-9]+\)
**  addi\t[a-x0-9]+,[a-x0-9]+,4
**  slli\t[a-x0-9]+,[a-x0-9]+,1
**  add\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
**  vl2re8.v\tv[0-9]+,0\([a-x0-9]+\)
**  vs2r.v\tv[0-9]+,0\([a-x0-9]+\)
**  addi\t[a-x0-9]+,[a-x0-9]+,5
**  slli\t[a-x0-9]+,[a-x0-9]+,2
**  add\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
**  vl4re8.v\tv[0-9]+,0\([a-x0-9]+\)
**  vs4r.v\tv[0-9]+,0\([a-x0-9]+\)
**  addi\t[a-x0-9]+,[a-x0-9]+,6
**  slli\t[a-x0-9]+,[a-x0-9]+,3
**  add\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
**  vl8re8.v\tv[0-9]+,0\([a-x0-9]+\)
**  vs8r.v\tv[0-9]+,0\([a-x0-9]+\)
**  csrr\t[a-x0-9]+,vlenb
**  slli\t[a-x0-9]+,[a-x0-9]+,4
**  add\tsp,[a-x0-9]+,[a-x0-9]+
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
