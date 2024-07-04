/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -mrvv-max-lmul=m8 -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "def.h"

/*
** mov0:
**	vsetivli\s+zero,\s*1,\s*e16,\s*mf4,\s*t[au],\s*m[au]
**	vle16\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov0 (int16_t *in, int16_t *out) 
{ 
 register v1hi v1 asm("v1") = *(v1hi*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v1hi v2 asm("v2") = v1; 
 *(v1hi*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov1:
**	vsetivli\s+zero,\s*2,\s*e16,\s*mf4,\s*t[au],\s*m[au]
**	vle16\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov1 (int16_t *in, int16_t *out) 
{ 
 register v2hi v1 asm("v1") = *(v2hi*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v2hi v2 asm("v2") = v1; 
 *(v2hi*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov2:
**	vsetivli\s+zero,\s*4,\s*e16,\s*mf4,\s*t[au],\s*m[au]
**	vle16\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov2 (int16_t *in, int16_t *out) 
{ 
 register v4hi v1 asm("v1") = *(v4hi*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v4hi v2 asm("v2") = v1; 
 *(v4hi*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov3:
**	vsetivli\s+zero,\s*8,\s*e16,\s*mf4,\s*t[au],\s*m[au]
**	vle16\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov3 (int16_t *in, int16_t *out) 
{ 
 register v8hi v1 asm("v1") = *(v8hi*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v8hi v2 asm("v2") = v1; 
 *(v8hi*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov4:
**	vsetivli\s+zero,\s*16,\s*e16,\s*mf4,\s*t[au],\s*m[au]
**	vle16\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov4 (int16_t *in, int16_t *out) 
{ 
 register v16hi v1 asm("v1") = *(v16hi*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v16hi v2 asm("v2") = v1; 
 *(v16hi*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov5:
**	li\s+[a-x0-9]+,32
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]
**	vle16\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov5 (int16_t *in, int16_t *out) 
{ 
 register v32hi v1 asm("v1") = *(v32hi*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v32hi v2 asm("v2") = v1; 
 *(v32hi*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov6:
**	li\s+[a-x0-9]+,64
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]
**	vle16\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov6 (int16_t *in, int16_t *out) 
{ 
 register v64hi v1 asm("v1") = *(v64hi*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v64hi v2 asm("v2") = v1; 
 *(v64hi*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov7:
**	li\s+[a-x0-9]+,128
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]
**	vle16\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov7 (int16_t *in, int16_t *out) 
{ 
 register v128hi v1 asm("v1") = *(v128hi*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v128hi v2 asm("v2") = v1; 
 *(v128hi*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov8:
**	li\s+[a-x0-9]+,256
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]
**	vle16\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov8 (int16_t *in, int16_t *out) 
{ 
 register v256hi v1 asm("v1") = *(v256hi*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v256hi v2 asm("v2") = v1; 
 *(v256hi*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov9:
**	li\s+[a-x0-9]+,512
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]
**	vle16\.v\tv2,0\s*\([a-x0-9]+\)
**  ...
**  vmv2r\.v\tv4,v2
**	...
**  ret
*/
void mov9 (int16_t *in, int16_t *out) 
{ 
 register v512hi v1 asm("v2") = *(v512hi*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v512hi v2 asm("v4") = v1; 
 *(v512hi*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov10:
**	li\s+[a-x0-9]+,1024
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]
**	vle16\.v\tv4,0\s*\([a-x0-9]+\)
**  ...
**  vmv4r\.v\tv8,v4
**	...
**  ret
*/
void mov10 (uint16_t *in, uint16_t *out) 
{ 
 register v1024hi v2 asm("v4") = *(v1024hi*)in; 
 asm volatile ("# %0"::"vr"(v2)); 
 register v1024hi v4 asm("v8") = v2; 
 *(v1024hi*)out = v4; 
 asm volatile ("# %0"::"vr"(v4)); 
}

/*
** mov11:
**	li\s+[a-x0-9]+,4096
**	addi\s+[a-x0-9]+,[a-x0-9]+,-2048
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]
**	vle16\.v\tv8,0\s*\([a-x0-9]+\)
**  ...
**  vmv8r\.v\tv16,v8
**	...
**  ret
*/
void mov11 (uint16_t *in, uint16_t *out) 
{ 
 register v2048hi v4 asm("v8") = *(v2048hi*)in; 
 asm volatile ("# %0"::"vr"(v4)); 
 register v2048hi v8 asm("v16") = v4; 
 *(v2048hi*)out = v8; 
 asm volatile ("# %0"::"vr"(v8)); 
}
