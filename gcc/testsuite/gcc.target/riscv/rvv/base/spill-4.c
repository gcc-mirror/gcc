/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -mpreferred-stack-boundary=3 -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"
#include "macro.h"

/*
** spill_4:
**  csrr\tt0,vlenb
**  sub\tsp,sp,t0
**  ...
**  vs1r.v\tv24,0\(sp\)
**  ...
**  vl1re64.v\tv2,0\(sp\)
**  vs1r.v\tv2,0\(a1\)
**  ...
**  jr\tra
*/
void
spill_4 (int64_t *in, int64_t *out)
{
  register vint64m1_t v1 asm("v1") = *(vint64m1_t*)in; 
  asm volatile ("# %0"::"vr"(v1)); 
  exhaust_vector_regs (); 
  register vint64m1_t v2 asm("v2") = v1; 
  *(vint64m1_t*)out = v2; 
  asm volatile ("# %0"::"vr"(v2));
}

/*
** spill_5:
**  csrr\tt0,vlenb
**  slli\tt1,t0,1
**  sub\tsp,sp,t1
**  ...
**  vs2r.v\tv24,0\(sp\)
**  ...
**  vl2re64.v\tv4,0\(sp\)
**  vs2r.v\tv4,0\(a1\)
**  ...
**  jr\tra
*/
void
spill_5 (int64_t *in, int64_t *out)
{
  register vint64m2_t v2 asm("v2") = *(vint64m2_t*)in; 
  asm volatile ("# %0"::"vr"(v2)); 
  exhaust_vector_regs (); 
  register vint64m2_t v4 asm("v4") = v2; 
  *(vint64m2_t*)out = v4; 
  asm volatile ("# %0"::"vr"(v4));
}

/*
** spill_6:
**  csrr\tt0,vlenb
**  slli\tt1,t0,2
**  sub\tsp,sp,t1
**  ...
**  vs4r.v\tv24,0\(sp\)
**  ...
**  vl4re64.v\tv8,0\(sp\)
**  vs4r.v\tv8,0\(a1\)
**  ...
**  jr\tra
*/
void
spill_6 (int64_t *in, int64_t *out)
{
  register vint64m4_t v4 asm("v4") = *(vint64m4_t*)in; 
  asm volatile ("# %0"::"vr"(v4)); 
  exhaust_vector_regs (); 
  register vint64m4_t v8 asm("v8") = v4; 
  *(vint64m4_t*)out = v8; 
  asm volatile ("# %0"::"vr"(v8));
}

/*
** spill_7:
**  csrr\tt0,vlenb
**  slli\tt1,t0,3
**  sub\tsp,sp,t1
**  ...
**  vs8r.v\tv24,0\(sp\)
**  ...
**  vl8re64.v\tv16,0\(sp\)
**  vs8r.v\tv16,0\(a1\)
**  ...
**  jr\tra
*/
void
spill_7 (int64_t *in, int64_t *out)
{
  register vint64m8_t v8 asm("v8") = *(vint64m8_t*)in; 
  asm volatile ("# %0"::"vr"(v8)); 
  exhaust_vector_regs (); 
  register vint64m8_t v16 asm("v16") = v8; 
  *(vint64m8_t*)out = v16; 
  asm volatile ("# %0"::"vr"(v16));
}

/*
** spill_11:
**  csrr\tt0,vlenb
**  sub\tsp,sp,t0
**  ...
**  vs1r.v\tv24,0\(sp\)
**  ...
**  vl1re64.v\tv2,0\(sp\)
**  vs1r.v\tv2,0\(a1\)
**  ...
**  jr\tra
*/
void
spill_11 (uint64_t *in, uint64_t *out)
{
  register vuint64m1_t v1 asm("v1") = *(vuint64m1_t*)in; 
  asm volatile ("# %0"::"vr"(v1)); 
  exhaust_vector_regs (); 
  register vuint64m1_t v2 asm("v2") = v1; 
  *(vuint64m1_t*)out = v2; 
  asm volatile ("# %0"::"vr"(v2));
}

/*
** spill_12:
**  csrr\tt0,vlenb
**  slli\tt1,t0,1
**  sub\tsp,sp,t1
**  ...
**  vs2r.v\tv24,0\(sp\)
**  ...
**  vl2re64.v\tv4,0\(sp\)
**  vs2r.v\tv4,0\(a1\)
**  ...
**  jr\tra
*/
void
spill_12 (uint64_t *in, uint64_t *out)
{
  register vuint64m2_t v2 asm("v2") = *(vuint64m2_t*)in; 
  asm volatile ("# %0"::"vr"(v2)); 
  exhaust_vector_regs (); 
  register vuint64m2_t v4 asm("v4") = v2; 
  *(vuint64m2_t*)out = v4; 
  asm volatile ("# %0"::"vr"(v4));
}

/*
** spill_13:
**  csrr\tt0,vlenb
**  slli\tt1,t0,2
**  sub\tsp,sp,t1
**  ...
**  vs4r.v\tv24,0\(sp\)
**  ...
**  vl4re64.v\tv8,0\(sp\)
**  vs4r.v\tv8,0\(a1\)
**  ...
**  jr\tra
*/
void
spill_13 (uint64_t *in, uint64_t *out)
{
  register vuint64m4_t v4 asm("v4") = *(vuint64m4_t*)in; 
  asm volatile ("# %0"::"vr"(v4)); 
  exhaust_vector_regs (); 
  register vuint64m4_t v8 asm("v8") = v4; 
  *(vuint64m4_t*)out = v8; 
  asm volatile ("# %0"::"vr"(v8));
}

/*
** spill_14:
**  csrr\tt0,vlenb
**  slli\tt1,t0,3
**  sub\tsp,sp,t1
**  ...
**  vs8r.v\tv24,0\(sp\)
**  ...
**  vl8re64.v\tv16,0\(sp\)
**  vs8r.v\tv16,0\(a1\)
**  ...
**  jr\tra
*/
void
spill_14 (uint64_t *in, uint64_t *out)
{
  register vuint64m8_t v8 asm("v8") = *(vuint64m8_t*)in; 
  asm volatile ("# %0"::"vr"(v8)); 
  exhaust_vector_regs (); 
  register vuint64m8_t v16 asm("v16") = v8; 
  *(vuint64m8_t*)out = v16; 
  asm volatile ("# %0"::"vr"(v16));
}
