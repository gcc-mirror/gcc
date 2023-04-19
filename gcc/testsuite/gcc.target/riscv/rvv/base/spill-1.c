/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zve64d -mabi=ilp32 -mpreferred-stack-boundary=3 -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"
#include "macro.h"

/*
** spill_1:
**  csrr\tt0,vlenb
**  sub\tsp,sp,t0
**  ...
**  csrr\t[a-x0-9]+,vlenb
**  srli\t[a-x0-9]+,[a-x0-9]+,3
**  slli\t[a-x0-9]+,[a-x0-9]+,3
**  sub\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
**  add\t[a-x0-9]+,[a-x0-9]+,sp
**  vse8.v\tv[0-9]+,0\([a-x0-9]+\)
**  ...
**  csrr\t[a-x0-9]+,vlenb
**  srli\t[a-x0-9]+,[a-x0-9]+,3
**  slli\t[a-x0-9]+,[a-x0-9]+,3
**  sub\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
**  add\t[a-x0-9]+,[a-x0-9]+,sp
**  vle8.v\tv[0-9]+,0\([a-x0-9]+\)
**  vse8.v\tv[0-9]+,0\([a-x0-9]+\)
**  csrr\tt0,vlenb
**  add\tsp,sp,t0
**  ...
**  jr\tra
*/
void
spill_1 (int8_t *in, int8_t *out)
{
  vint8mf8_t v1 = *(vint8mf8_t*)in;
  exhaust_vector_regs ();
  *(vint8mf8_t*)out = v1;
}

/*
** spill_2:
**  csrr\tt0,vlenb
**  sub\tsp,sp,t0
**  vsetvli\ta5,zero,e8,mf4,ta,ma
**  vle8.v\tv[0-9]+,0\(a0\)
**  csrr\t[a-x0-9]+,vlenb
**  srli\t[a-x0-9]+,[a-x0-9]+,2
**  slli\t[a-x0-9]+,[a-x0-9]+,2
**  sub\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
**  add\t[a-x0-9]+,[a-x0-9]+,sp
**  vse8.v\tv[0-9]+,0\([a-x0-9]+\)
**  ...
**  csrr\t[a-x0-9]+,vlenb
**  srli\t[a-x0-9]+,[a-x0-9]+,2
**  slli\t[a-x0-9]+,[a-x0-9]+,2
**  sub\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
**  add\t[a-x0-9]+,[a-x0-9]+,sp
**  vle8.v\tv[0-9]+,0\([a-x0-9]+\)
**  vse8.v\tv[0-9]+,0\([a-x0-9]+\)
**  csrr\tt0,vlenb
**  add\tsp,sp,t0
**  ...
**  jr\tra
*/
void
spill_2 (int8_t *in, int8_t *out)
{
  vint8mf4_t v1 = *(vint8mf4_t*)in;
  exhaust_vector_regs ();
  *(vint8mf4_t*)out = v1;
}

/*
** spill_3:
** csrr\tt0,vlenb
** sub\tsp,sp,t0
** vsetvli\ta5,zero,e8,mf2,ta,ma
** vle8.v\tv[0-9]+,0\(a0\)
** csrr\t[a-x0-9]+,vlenb
** srli\t[a-x0-9]+,[a-x0-9]+,1
** add\t[a-x0-9]+,[a-x0-9]+,sp
** vse8.v\tv[0-9]+,0\([a-x0-9]+\)
**  ...
** csrr\t[a-x0-9]+,vlenb
** srli\t[a-x0-9]+,[a-x0-9]+,1
** add\t[a-x0-9]+,[a-x0-9]+,sp
** vle8.v\tv[0-9]+,0\([a-x0-9]+\)
** vse8.v\tv[0-9]+,0\([a-x0-9]+\)
** csrr\tt0,vlenb
** add\tsp,sp,t0
**  ...
**  jr\tra
*/
void
spill_3 (int8_t *in, int8_t *out)
{
  vint8mf2_t v1 = *(vint8mf2_t*)in;
  exhaust_vector_regs ();
  *(vint8mf2_t*)out = v1;
}

/*
** spill_4:
**  csrr\tt0,vlenb
**  sub\tsp,sp,t0
**  ...
**  vs1r.v\tv[0-9]+,0\(sp\)
**  ...
**  vl1re8.v\tv2,0\(sp\)
**  vs1r.v\tv2,0\([a-x0-9]+\)
**  ...
**  jr\tra
*/
void
spill_4 (int8_t *in, int8_t *out)
{
  register vint8m1_t v1 asm("v1") = *(vint8m1_t*)in; 
  asm volatile ("# %0"::"vr"(v1)); 
  exhaust_vector_regs (); 
  register vint8m1_t v2 asm("v2") = v1; 
  *(vint8m1_t*)out = v2; 
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
**  vl2re8.v\tv4,0\(sp\)
**  vs2r.v\tv4,0\([a-x0-9]+\)
**  ...
**  jr\tra
*/
void
spill_5 (int8_t *in, int8_t *out)
{
  register vint8m2_t v2 asm("v2") = *(vint8m2_t*)in; 
  asm volatile ("# %0"::"vr"(v2)); 
  exhaust_vector_regs (); 
  register vint8m2_t v4 asm("v4") = v2; 
  *(vint8m2_t*)out = v4; 
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
**  vl4re8.v\tv8,0\(sp\)
**  vs4r.v\tv8,0\([a-x0-9]+\)
**  ...
**  jr\tra
*/
void
spill_6 (int8_t *in, int8_t *out)
{
  register vint8m4_t v4 asm("v4") = *(vint8m4_t*)in; 
  asm volatile ("# %0"::"vr"(v4)); 
  exhaust_vector_regs (); 
  register vint8m4_t v8 asm("v8") = v4; 
  *(vint8m4_t*)out = v8; 
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
**  vl8re8.v\tv16,0\(sp\)
**  vs8r.v\tv16,0\([a-x0-9]+\)
**  ...
**  jr\tra
*/
void
spill_7 (int8_t *in, int8_t *out)
{
  register vint8m8_t v8 asm("v8") = *(vint8m8_t*)in; 
  asm volatile ("# %0"::"vr"(v8)); 
  exhaust_vector_regs (); 
  register vint8m8_t v16 asm("v16") = v8; 
  *(vint8m8_t*)out = v16; 
  asm volatile ("# %0"::"vr"(v16));
}

/*
** spill_8:
** csrr\tt0,vlenb
** sub\tsp,sp,t0
** vsetvli\ta5,zero,e8,mf8,ta,ma
** vle8.v\tv[0-9]+,0\(a0\)
** csrr\t[a-x0-9]+,vlenb
** srli\t[a-x0-9]+,[a-x0-9]+,3
** slli\t[a-x0-9]+,[a-x0-9]+,3
** sub\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
** add\t[a-x0-9]+,[a-x0-9]+,sp
** vse8.v\tv[0-9]+,0\([a-x0-9]+\)
**  ...
**  csrr\t[a-x0-9]+,vlenb
**  srli\t[a-x0-9]+,[a-x0-9]+,3
**  slli\t[a-x0-9]+,[a-x0-9]+,3
**  sub\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
**  add\t[a-x0-9]+,[a-x0-9]+,sp
**  vle8.v\tv[0-9]+,0\([a-x0-9]+\)
**  vse8.v\tv[0-9]+,0\([a-x0-9]+\)
**  csrr\tt0,vlenb
**  add\tsp,sp,t0
**  ...
**  jr\tra
*/
void
spill_8 (uint8_t *in, uint8_t *out)
{
  vuint8mf8_t v1 = *(vuint8mf8_t*)in;
  exhaust_vector_regs ();
  *(vuint8mf8_t*)out = v1;
}

/*
** spill_9:
**  csrr\tt0,vlenb
**  sub\tsp,sp,t0
**  vsetvli\ta5,zero,e8,mf4,ta,ma
**  vle8.v\tv[0-9]+,0\(a0\)
**  csrr\t[a-x0-9]+,vlenb
**  srli\t[a-x0-9]+,[a-x0-9]+,2
**  slli\t[a-x0-9]+,[a-x0-9]+,2
**  sub\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
**  add\t[a-x0-9]+,[a-x0-9]+,sp
**  vse8.v\tv[0-9]+,0\([a-x0-9]+\)
**  ...
**  csrr\t[a-x0-9]+,vlenb
**  srli\t[a-x0-9]+,[a-x0-9]+,2
**  slli\t[a-x0-9]+,[a-x0-9]+,2
**  sub\t[a-x0-9]+,[a-x0-9]+,[a-x0-9]+
**  add\t[a-x0-9]+,[a-x0-9]+,sp
**  vle8.v\tv[0-9]+,0\([a-x0-9]+\)
**  vse8.v\tv[0-9]+,0\([a-x0-9]+\)
**  csrr\tt0,vlenb
**  add\tsp,sp,t0
**  ...
**  jr\tra
*/
void
spill_9 (uint8_t *in, uint8_t *out)
{
  vuint8mf4_t v1 = *(vuint8mf4_t*)in;
  exhaust_vector_regs ();
  *(vuint8mf4_t*)out = v1;
}

/*
** spill_10:
** csrr\tt0,vlenb
** sub\tsp,sp,t0
** vsetvli\ta5,zero,e8,mf2,ta,ma
** vle8.v\tv[0-9]+,0\(a0\)
** csrr\t[a-x0-9]+,vlenb
** srli\t[a-x0-9]+,[a-x0-9]+,1
** add\t[a-x0-9]+,[a-x0-9]+,sp
** vse8.v\tv[0-9]+,0\([a-x0-9]+\)
**  ...
**  csrr\t[a-x0-9]+,vlenb
**  srli\t[a-x0-9]+,[a-x0-9]+,1
**  add\t[a-x0-9]+,[a-x0-9]+,sp
**  vle8.v\tv[0-9]+,0\([a-x0-9]+\)
**  vse8.v\tv[0-9]+,0\([a-x0-9]+\)
**  csrr\tt0,vlenb
**  add\tsp,sp,t0
**  ...
**  jr\tra
*/
void
spill_10 (uint8_t *in, uint8_t *out)
{
  vuint8mf2_t v1 = *(vuint8mf2_t*)in;
  exhaust_vector_regs ();
  *(vuint8mf2_t*)out = v1;
}

/*
** spill_11:
**  csrr\tt0,vlenb
**  sub\tsp,sp,t0
**  ...
**  vs1r.v\tv[0-9]+,0\(sp\)
**  ...
**  vl1re8.v\tv2,0\(sp\)
**  vs1r.v\tv2,0\([a-x0-9]+\)
**  ...
**  jr\tra
*/
void
spill_11 (uint8_t *in, uint8_t *out)
{
  register vuint8m1_t v1 asm("v1") = *(vuint8m1_t*)in; 
  asm volatile ("# %0"::"vr"(v1)); 
  exhaust_vector_regs (); 
  register vuint8m1_t v2 asm("v2") = v1; 
  *(vuint8m1_t*)out = v2; 
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
**  vl2re8.v\tv4,0\(sp\)
**  vs2r.v\tv4,0\([a-x0-9]+\)
**  ...
**  jr\tra
*/
void
spill_12 (uint8_t *in, uint8_t *out)
{
  register vuint8m2_t v2 asm("v2") = *(vuint8m2_t*)in; 
  asm volatile ("# %0"::"vr"(v2)); 
  exhaust_vector_regs (); 
  register vuint8m2_t v4 asm("v4") = v2; 
  *(vuint8m2_t*)out = v4; 
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
**  vl4re8.v\tv8,0\(sp\)
**  vs4r.v\tv8,0\([a-x0-9]+\)
**  ...
**  jr\tra
*/
void
spill_13 (uint8_t *in, uint8_t *out)
{
  register vuint8m4_t v4 asm("v4") = *(vuint8m4_t*)in; 
  asm volatile ("# %0"::"vr"(v4)); 
  exhaust_vector_regs (); 
  register vuint8m4_t v8 asm("v8") = v4; 
  *(vuint8m4_t*)out = v8; 
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
**  vl8re8.v\tv16,0\(sp\)
**  vs8r.v\tv16,0\([a-x0-9]+\)
**  ...
**  jr\tra
*/
void
spill_14 (uint8_t *in, uint8_t *out)
{
  register vuint8m8_t v8 asm("v8") = *(vuint8m8_t*)in; 
  asm volatile ("# %0"::"vr"(v8)); 
  exhaust_vector_regs (); 
  register vuint8m8_t v16 asm("v16") = v8; 
  *(vuint8m8_t*)out = v16; 
  asm volatile ("# %0"::"vr"(v16));
}
