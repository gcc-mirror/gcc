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
void mov0 (_Float16 *in, _Float16 *out) 
{ 
 register v1hf v1 asm("v1") = *(v1hf*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v1hf v2 asm("v2") = v1; 
 *(v1hf*)out = v2; 
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
void mov1 (_Float16 *in, _Float16 *out) 
{ 
 register v2hf v1 asm("v1") = *(v2hf*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v2hf v2 asm("v2") = v1; 
 *(v2hf*)out = v2; 
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
void mov2 (_Float16 *in, _Float16 *out) 
{ 
 register v4hf v1 asm("v1") = *(v4hf*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v4hf v2 asm("v2") = v1; 
 *(v4hf*)out = v2; 
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
void mov3 (_Float16 *in, _Float16 *out) 
{ 
 register v8hf v1 asm("v1") = *(v8hf*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v8hf v2 asm("v2") = v1; 
 *(v8hf*)out = v2; 
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
void mov4 (_Float16 *in, _Float16 *out) 
{ 
 register v16hf v1 asm("v1") = *(v16hf*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v16hf v2 asm("v2") = v1; 
 *(v16hf*)out = v2; 
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
void mov5 (_Float16 *in, _Float16 *out) 
{ 
 register v32hf v1 asm("v1") = *(v32hf*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v32hf v2 asm("v2") = v1; 
 *(v32hf*)out = v2; 
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
void mov6 (_Float16 *in, _Float16 *out) 
{ 
 register v64hf v1 asm("v1") = *(v64hf*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v64hf v2 asm("v2") = v1; 
 *(v64hf*)out = v2; 
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
void mov7 (_Float16 *in, _Float16 *out) 
{ 
 register v128hf v1 asm("v1") = *(v128hf*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v128hf v2 asm("v2") = v1; 
 *(v128hf*)out = v2; 
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
void mov8 (_Float16 *in, _Float16 *out) 
{ 
 register v256hf v1 asm("v1") = *(v256hf*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v256hf v2 asm("v2") = v1; 
 *(v256hf*)out = v2; 
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
void mov9 (_Float16 *in, _Float16 *out) 
{ 
 register v512hf v1 asm("v2") = *(v512hf*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v512hf v2 asm("v4") = v1; 
 *(v512hf*)out = v2; 
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
void mov10 (_Float16 *in, _Float16 *out) 
{ 
 register v1024hf v2 asm("v4") = *(v1024hf*)in; 
 asm volatile ("# %0"::"vr"(v2)); 
 register v1024hf v4 asm("v8") = v2; 
 *(v1024hf*)out = v4; 
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
void mov11 (_Float16 *in, _Float16 *out) 
{ 
 register v2048hf v4 asm("v8") = *(v2048hf*)in; 
 asm volatile ("# %0"::"vr"(v4)); 
 register v2048hf v8 asm("v16") = v4; 
 *(v2048hf*)out = v8; 
 asm volatile ("# %0"::"vr"(v8)); 
}
