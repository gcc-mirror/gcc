/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -mpreferred-stack-boundary=3 -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"
#include "macro.h"

/*
** spill_3:
**  csrr\tt0,vlenb
**  sub\tsp,sp,t0
**  vsetvli\ta5,zero,e32,mf2,ta,ma
**  vle32.v\tv[0-9]+,0\(a0\)
**  csrr\t[a-x0-9]+,vlenb
**  srli\t[a-x0-9]+,[a-x0-9]+,1
**  add\t[a-x0-9]+,[a-x0-9]+,sp
**  vse32.v\tv[0-9]+,0\([a-x0-9]+\)
**  ...
**  csrr\t[a-x0-9]+,vlenb
**  srli\t[a-x0-9]+,[a-x0-9]+,1
**  add\t[a-x0-9]+,[a-x0-9]+,sp
**  vle32.v\tv[0-9]+,0\([a-x0-9]+\)
**  vse32.v\tv[0-9]+,0\([a-x0-9]+\)
**  csrr\tt0,vlenb
**  add\tsp,sp,t0
**  ...
**  jr\tra
*/
void
spill_3 (float *in, float *out)
{
  vfloat32mf2_t v1 = *(vfloat32mf2_t*)in;
  exhaust_vector_regs ();
  *(vfloat32mf2_t*)out = v1;
}

/*
** spill_4:
**  csrr\tt0,vlenb
**  sub\tsp,sp,t0
**  ...
**  vs1r.v\tv[0-9]+,0\(sp\)
**  ...
**  vl1re32.v\tv2,0\(sp\)
**  vs1r.v\tv2,0\([a-x0-9]+\)
**  ...
**  jr\tra
*/
void
spill_4 (float *in, float *out)
{
  register vfloat32m1_t v1 asm("v1") = *(vfloat32m1_t*)in; 
  asm volatile ("# %0"::"vr"(v1)); 
  exhaust_vector_regs (); 
  register vfloat32m1_t v2 asm("v2") = v1; 
  *(vfloat32m1_t*)out = v2; 
  asm volatile ("# %0"::"vr"(v2));
}

/*
** spill_5:
**  csrr\tt0,vlenb
**  slli\tt1,t0,1
**  sub\tsp,sp,t1
**  ...
**  vs2r.v\tv[0-9]+,0\(sp\)
**  ...
**  vl2re32.v\tv4,0\(sp\)
**  vs2r.v\tv4,0\([a-x0-9]+\)
**  ...
**  jr\tra
*/
void
spill_5 (float *in, float *out)
{
  register vfloat32m2_t v2 asm("v2") = *(vfloat32m2_t*)in; 
  asm volatile ("# %0"::"vr"(v2)); 
  exhaust_vector_regs (); 
  register vfloat32m2_t v4 asm("v4") = v2; 
  *(vfloat32m2_t*)out = v4; 
  asm volatile ("# %0"::"vr"(v4));
}

/*
** spill_6:
**  csrr\tt0,vlenb
**  slli\tt1,t0,2
**  sub\tsp,sp,t1
**  ...
**  vs4r.v\tv[0-9]+,0\(sp\)
**  ...
**  vl4re32.v\tv8,0\(sp\)
**  vs4r.v\tv8,0\([a-x0-9]+\)
**  ...
**  jr\tra
*/
void
spill_6 (float *in, float *out)
{
  register vfloat32m4_t v4 asm("v4") = *(vfloat32m4_t*)in; 
  asm volatile ("# %0"::"vr"(v4)); 
  exhaust_vector_regs (); 
  register vfloat32m4_t v8 asm("v8") = v4; 
  *(vfloat32m4_t*)out = v8; 
  asm volatile ("# %0"::"vr"(v8));
}

/*
** spill_7:
**  csrr\tt0,vlenb
**  slli\tt1,t0,3
**  sub\tsp,sp,t1
**  ...
**  vs8r.v\tv[0-9]+,0\(sp\)
**  ...
**  vl8re32.v\tv16,0\(sp\)
**  vs8r.v\tv16,0\([a-x0-9]+\)
**  ...
**  jr\tra
*/
void
spill_7 (float *in, float *out)
{
  register vfloat32m8_t v8 asm("v8") = *(vfloat32m8_t*)in; 
  asm volatile ("# %0"::"vr"(v8)); 
  exhaust_vector_regs (); 
  register vfloat32m8_t v16 asm("v16") = v8; 
  *(vfloat32m8_t*)out = v16; 
  asm volatile ("# %0"::"vr"(v16));
}
