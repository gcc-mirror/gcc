/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zve64d -mabi=ilp32 -mpreferred-stack-boundary=3 -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"
#include "macro.h"

/*
** spill_2:
**  csrr\tt0,vlenb
**  sub\tsp,sp,t0
**  vsetvli\ta5,zero,e16,mf4,ta,ma
**  vle16.v\tv[0-9]+,0\(a0\)
**  csrr\t[a-x0-9]+,vlenb
**  srli\t[a-x0-9]+,[a-x0-9]+,2
**  slli\t[a-x0-9]+,[a-x0-9]+,2
**  sub\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
**  add\t[a-x0-9]+,[a-x0-9]+,sp
**  vse16.v\tv[0-9]+,0\([a-x0-9]+\)
**  ...
**  csrr\t[a-x0-9]+,vlenb
**  srli\t[a-x0-9]+,[a-x0-9]+,2
**  slli\t[a-x0-9]+,[a-x0-9]+,2
**  sub\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
**  add\t[a-x0-9]+,[a-x0-9]+,sp
**  vle16.v\tv[0-9]+,0\([a-x0-9]+\)
**  vse16.v\tv[0-9]+,0\([a-x0-9]+\)
**  csrr\tt0,vlenb
**  add\tsp,sp,t0
**  ...
**  jr\tra
*/
void
spill_2 (int16_t *in, int16_t *out)
{
  vint16mf4_t v1 = *(vint16mf4_t*)in;
  exhaust_vector_regs ();
  *(vint16mf4_t*)out = v1;
}

/*
** spill_3:
**  csrr\tt0,vlenb
**  sub\tsp,sp,t0
**  vsetvli\ta5,zero,e16,mf2,ta,ma
**  vle16.v\tv[0-9]+,0\(a0\)
**  csrr\t[a-x0-9]+,vlenb
**  srli\t[a-x0-9]+,[a-x0-9]+,1
**  add\t[a-x0-9]+,[a-x0-9]+,sp
**  vse16.v\tv[0-9]+,0\([a-x0-9]+\)
**  ...
**  csrr\t[a-x0-9]+,vlenb
**  srli\t[a-x0-9]+,[a-x0-9]+,1
**  add\t[a-x0-9]+,[a-x0-9]+,sp
**  vle16.v\tv[0-9]+,0\([a-x0-9]+\)
**  vse16.v\tv[0-9]+,0\([a-x0-9]+\)
**  csrr\tt0,vlenb
**  add\tsp,sp,t0
**  ...
**  jr\tra
*/
void
spill_3 (int16_t *in, int16_t *out)
{
  vint16mf2_t v1 = *(vint16mf2_t*)in;
  exhaust_vector_regs ();
  *(vint16mf2_t*)out = v1;
}

/*
** spill_4:
**  csrr\tt0,vlenb
**  sub\tsp,sp,t0
**  ...
**  vs1r.v\tv[0-9]+,0\(sp\)
**  ...
**  vl1re16.v\tv2,0\(sp\)
**  vs1r.v\tv2,0\([a-x0-9]+\)
**  ...
**  jr\tra
*/
void
spill_4 (int16_t *in, int16_t *out)
{
  register vint16m1_t v1 asm("v1") = *(vint16m1_t*)in; 
  asm volatile ("# %0"::"vr"(v1)); 
  exhaust_vector_regs (); 
  register vint16m1_t v2 asm("v2") = v1; 
  *(vint16m1_t*)out = v2; 
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
**  vl2re16.v\tv4,0\(sp\)
**  vs2r.v\tv4,0\([a-x0-9]+\)
**  ...
**  jr\tra
*/
void
spill_5 (int16_t *in, int16_t *out)
{
  register vint16m2_t v2 asm("v2") = *(vint16m2_t*)in; 
  asm volatile ("# %0"::"vr"(v2)); 
  exhaust_vector_regs (); 
  register vint16m2_t v4 asm("v4") = v2; 
  *(vint16m2_t*)out = v4; 
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
**  vl4re16.v\tv8,0\(sp\)
**  vs4r.v\tv8,0\([a-x0-9]+\)
**  ...
**  jr\tra
*/
void
spill_6 (int16_t *in, int16_t *out)
{
  register vint16m4_t v4 asm("v4") = *(vint16m4_t*)in; 
  asm volatile ("# %0"::"vr"(v4)); 
  exhaust_vector_regs (); 
  register vint16m4_t v8 asm("v8") = v4; 
  *(vint16m4_t*)out = v8; 
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
**  vl8re16.v\tv16,0\(sp\)
**  vs8r.v\tv16,0\([a-x0-9]+\)
**  ...
**  jr\tra
*/
void
spill_7 (int16_t *in, int16_t *out)
{
  register vint16m8_t v8 asm("v8") = *(vint16m8_t*)in; 
  asm volatile ("# %0"::"vr"(v8)); 
  exhaust_vector_regs (); 
  register vint16m8_t v16 asm("v16") = v8; 
  *(vint16m8_t*)out = v16; 
  asm volatile ("# %0"::"vr"(v16));
}

/*
** spill_9:
**  csrr\tt0,vlenb
**  sub\tsp,sp,t0
**  vsetvli\ta5,zero,e16,mf4,ta,ma
**  vle16.v\tv[0-9]+,0\(a0\)
**  csrr\t[a-x0-9]+,vlenb
**  srli\t[a-x0-9]+,[a-x0-9]+,2
**  slli\t[a-x0-9]+,[a-x0-9]+,2
**  sub\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
**  add\t[a-x0-9]+,[a-x0-9]+,sp
**  vse16.v\tv[0-9]+,0\([a-x0-9]+\)
**  ...
**  csrr\t[a-x0-9]+,vlenb
**  srli\t[a-x0-9]+,[a-x0-9]+,2
**  slli\t[a-x0-9]+,[a-x0-9]+,2
**  sub\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
**  add\t[a-x0-9]+,[a-x0-9]+,sp
**  vle16.v\tv[0-9]+,0\([a-x0-9]+\)
**  vse16.v\tv[0-9]+,0\([a-x0-9]+\)
**  csrr\tt0,vlenb
**  add\tsp,sp,t0
**  ...
**  jr\tra
*/
void
spill_9 (uint16_t *in, uint16_t *out)
{
  vuint16mf4_t v1 = *(vuint16mf4_t*)in;
  exhaust_vector_regs ();
  *(vuint16mf4_t*)out = v1;
}

/*
** spill_10:
**  csrr\tt0,vlenb
**  sub\tsp,sp,t0
**  vsetvli\ta5,zero,e16,mf2,ta,ma
**  vle16.v\tv[0-9]+,0\(a0\)
**  csrr\t[a-x0-9]+,vlenb
**  srli\t[a-x0-9]+,[a-x0-9]+,1
**  add\t[a-x0-9]+,[a-x0-9]+,sp
**  vse16.v\tv[0-9]+,0\([a-x0-9]+\)
**  ...
**  csrr\t[a-x0-9]+,vlenb
**  srli\t[a-x0-9]+,[a-x0-9]+,1
**  add\t[a-x0-9]+,[a-x0-9]+,sp
**  vle16.v\tv[0-9]+,0\([a-x0-9]+\)
**  vse16.v\tv[0-9]+,0\([a-x0-9]+\)
**  csrr\tt0,vlenb
**  add\tsp,sp,t0
**  ...
**  jr\tra
*/
void
spill_10 (uint16_t *in, uint16_t *out)
{
  vuint16mf2_t v1 = *(vuint16mf2_t*)in;
  exhaust_vector_regs ();
  *(vuint16mf2_t*)out = v1;
}

/*
** spill_11:
**  csrr\tt0,vlenb
**  sub\tsp,sp,t0
**  ...
**  vs1r.v\tv[0-9]+,0\(sp\)
**  ...
**  vl1re16.v\tv2,0\(sp\)
**  vs1r.v\tv2,0\([a-x0-9]+\)
**  ...
**  jr\tra
*/
void
spill_11 (uint16_t *in, uint16_t *out)
{
  register vuint16m1_t v1 asm("v1") = *(vuint16m1_t*)in; 
  asm volatile ("# %0"::"vr"(v1)); 
  exhaust_vector_regs (); 
  register vuint16m1_t v2 asm("v2") = v1; 
  *(vuint16m1_t*)out = v2; 
  asm volatile ("# %0"::"vr"(v2));
}

/*
** spill_12:
**  csrr\tt0,vlenb
**  slli\tt1,t0,1
**  sub\tsp,sp,t1
**  ...
**  vs2r.v\tv[0-9]+,0\(sp\)
**  ...
**  vl2re16.v\tv4,0\(sp\)
**  vs2r.v\tv4,0\([a-x0-9]+\)
**  ...
**  jr\tra
*/
void
spill_12 (uint16_t *in, uint16_t *out)
{
  register vuint16m2_t v2 asm("v2") = *(vuint16m2_t*)in; 
  asm volatile ("# %0"::"vr"(v2)); 
  exhaust_vector_regs (); 
  register vuint16m2_t v4 asm("v4") = v2; 
  *(vuint16m2_t*)out = v4; 
  asm volatile ("# %0"::"vr"(v4));
}

/*
** spill_13:
**  csrr\tt0,vlenb
**  slli\tt1,t0,2
**  sub\tsp,sp,t1
**  ...
**  vs4r.v\tv[0-9]+,0\(sp\)
**  ...
**  vl4re16.v\tv8,0\(sp\)
**  vs4r.v\tv8,0\([a-x0-9]+\)
**  ...
**  jr\tra
*/
void
spill_13 (uint16_t *in, uint16_t *out)
{
  register vuint16m4_t v4 asm("v4") = *(vuint16m4_t*)in; 
  asm volatile ("# %0"::"vr"(v4)); 
  exhaust_vector_regs (); 
  register vuint16m4_t v8 asm("v8") = v4; 
  *(vuint16m4_t*)out = v8; 
  asm volatile ("# %0"::"vr"(v8));
}

/*
** spill_14:
**  csrr\tt0,vlenb
**  slli\tt1,t0,3
**  sub\tsp,sp,t1
**  ...
**  vs8r.v\tv[0-9]+,0\(sp\)
**  ...
**  vl8re16.v\tv16,0\(sp\)
**  vs8r.v\tv16,0\([a-x0-9]+\)
**  ...
**  jr\tra
*/
void
spill_14 (uint16_t *in, uint16_t *out)
{
  register vuint16m8_t v8 asm("v8") = *(vuint16m8_t*)in; 
  asm volatile ("# %0"::"vr"(v8)); 
  exhaust_vector_regs (); 
  register vuint16m8_t v16 asm("v16") = v8; 
  *(vuint16m8_t*)out = v16; 
  asm volatile ("# %0"::"vr"(v16));
}
