/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h" 

/*
** mov14:
**	vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]
**	vle32\.v\tv1,0\s*\([a-x0-9]+\)
**	...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov14 (float *in, float *out) 
{ 
 register vfloat32mf2_t v1 asm("v1") = *(vfloat32mf2_t*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register vfloat32mf2_t v2 asm("v2") = v1; 
 *(vfloat32mf2_t*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov15:
**	vl1re32\.v\tv1,0\s*\([a-x0-9]+\)
**	...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov15 (float *in, float *out) 
{ 
 register vfloat32m1_t v1 asm("v1") = *(vfloat32m1_t*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register vfloat32m1_t v2 asm("v2") = v1; 
 *(vfloat32m1_t*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov16:
**	vl2re32\.v\tv2,0\s*\([a-x0-9]+\)
**	...
**  vmv2r\.v\tv4,v2
**	...
**  ret
*/
void mov16 (float *in, float *out) 
{ 
 register vfloat32m2_t v2 asm("v2") = *(vfloat32m2_t*)in; 
 asm volatile ("# %0"::"vr"(v2)); 
 register vfloat32m2_t v4 asm("v4") = v2; 
 *(vfloat32m2_t*)out = v4; 
 asm volatile ("# %0"::"vr"(v4)); 
}

/*
** mov17:
**	vl4re32\.v\tv4,0\s*\([a-x0-9]+\)
**	...
**  vmv4r\.v\tv8,v4
**	...
**  ret
*/
void mov17 (float *in, float *out)
{ 
 register vfloat32m4_t v4 asm("v4") = *(vfloat32m4_t*)in; 
 asm volatile ("# %0"::"vr"(v4)); 
 register vfloat32m4_t v8 asm("v8") = v4; 
 *(vfloat32m4_t*)out = v8; 
 asm volatile ("# %0"::"vr"(v8)); 
}

/*
** mov18:
**	vl8re32\.v\tv8,0\s*\([a-x0-9]+\)
**	...
**  vmv8r\.v\tv16,v8
**	...
**  ret
*/
void mov18 (float *in, float *out) 
{ 
 register vfloat32m8_t v8 asm("v8") = *(vfloat32m8_t*)in; 
 asm volatile ("# %0"::"vr"(v8)); 
 register vfloat32m8_t v16 asm("v16") = v8; 
 *(vfloat32m8_t*)out = v16; 
 asm volatile ("# %0"::"vr"(v16)); 
}

/*
** mov19:
**	vl1re64\.v\tv1,0\s*\([a-x0-9]+\)
**	...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov19 (uint64_t *in, uint64_t *out) 
{ 
 register vfloat64m1_t v1 asm("v1") = *(vfloat64m1_t*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register vfloat64m1_t v2 asm("v2") = v1; 
 *(vfloat64m1_t*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov20:
**	vl2re64\.v\tv2,0\s*\([a-x0-9]+\)
**	...
**  vmv2r\.v\tv4,v2
**	...
**  ret
*/
void mov20 (uint64_t *in, uint64_t *out) 
{ 
 register vfloat64m2_t v2 asm("v2") = *(vfloat64m2_t*)in; 
 asm volatile ("# %0"::"vr"(v2)); 
 register vfloat64m2_t v4 asm("v4") = v2; 
 *(vfloat64m2_t*)out = v4; 
 asm volatile ("# %0"::"vr"(v4)); 
}

/*
** mov21:
**	vl4re64\.v\tv4,0\s*\([a-x0-9]+\)
**	...
**  vmv4r\.v\tv8,v4
**	...
**  ret
*/
void mov21 (uint64_t *in, uint64_t *out) 
{ 
 register vfloat64m4_t v4 asm("v4") = *(vfloat64m4_t*)in; 
 asm volatile ("# %0"::"vr"(v4)); 
 register vfloat64m4_t v8 asm("v8") = v4; 
 *(vfloat64m4_t*)out = v8; 
 asm volatile ("# %0"::"vr"(v8)); 
}

/*
** mov22:
**	vl8re64\.v\tv8,0\s*\([a-x0-9]+\)
**	...
**  vmv8r\.v\tv16,v8
**	...
**  ret
*/
void mov22 (uint64_t *in, uint64_t *out) 
{ 
 register vfloat64m8_t v8 asm("v8") = *(vfloat64m8_t*)in; 
 asm volatile ("# %0"::"vr"(v8)); 
 register vfloat64m8_t v16 asm("v16") = v8; 
 *(vfloat64m8_t*)out = v16; 
 asm volatile ("# %0"::"vr"(v16)); 
}
