/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -mrvv-max-lmul=m8 -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "def.h"

/*
** mov0:
**	vsetivli\s+zero,\s*1,\s*e32,\s*mf2,\s*t[au],\s*m[au]
**	vle32\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov0 (float *in, float *out) 
{ 
 register v1sf v1 asm("v1") = *(v1sf*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v1sf v2 asm("v2") = v1; 
 *(v1sf*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov1:
**	vsetivli\s+zero,\s*2,\s*e32,\s*mf2,\s*t[au],\s*m[au]
**	vle32\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov1 (float *in, float *out) 
{ 
 register v2sf v1 asm("v1") = *(v2sf*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v2sf v2 asm("v2") = v1; 
 *(v2sf*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov2:
**	vsetivli\s+zero,\s*4,\s*e32,\s*mf2,\s*t[au],\s*m[au]
**	vle32\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov2 (float *in, float *out) 
{ 
 register v4sf v1 asm("v1") = *(v4sf*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v4sf v2 asm("v2") = v1; 
 *(v4sf*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov3:
**	vsetivli\s+zero,\s*8,\s*e32,\s*mf2,\s*t[au],\s*m[au]
**	vle32\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov3 (float *in, float *out) 
{ 
 register v8sf v1 asm("v1") = *(v8sf*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v8sf v2 asm("v2") = v1; 
 *(v8sf*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov4:
**	vsetivli\s+zero,\s*16,\s*e32,\s*mf2,\s*t[au],\s*m[au]
**	vle32\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov4 (float *in, float *out) 
{ 
 register v16sf v1 asm("v1") = *(v16sf*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v16sf v2 asm("v2") = v1; 
 *(v16sf*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov5:
**	li\s+[a-x0-9]+,32
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]
**	vle32\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov5 (float *in, float *out) 
{ 
 register v32sf v1 asm("v1") = *(v32sf*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v32sf v2 asm("v2") = v1; 
 *(v32sf*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov6:
**	li\s+[a-x0-9]+,64
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]
**	vle32\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov6 (float *in, float *out) 
{ 
 register v64sf v1 asm("v1") = *(v64sf*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v64sf v2 asm("v2") = v1; 
 *(v64sf*)out = v2; 
 asm volatile ("# %0"::"vr"(v2));
}

/*
** mov7:
**	li\s+[a-x0-9]+,128
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]
**	vle32\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov7 (float *in, float *out) 
{ 
 register v128sf v1 asm("v1") = *(v128sf*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v128sf v2 asm("v2") = v1; 
 *(v128sf*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov8:
**	li\s+[a-x0-9]+,256
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]
**	vle32\.v\tv2,0\s*\([a-x0-9]+\)
**  ...
**  vmv2r\.v\tv4,v2
**	...
**  ret
*/
void mov8 (float *in, float *out) 
{ 
 register v256sf v1 asm("v2") = *(v256sf*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v256sf v2 asm("v4") = v1; 
 *(v256sf*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov9:
**	li\s+[a-x0-9]+,512
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]
**	vle32\.v\tv4,0\s*\([a-x0-9]+\)
**  ...
**  vmv4r\.v\tv8,v4
**	...
**  ret
*/
void mov9 (float *in, float *out) 
{ 
 register v512sf v1 asm("v4") = *(v512sf*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v512sf v2 asm("v8") = v1; 
 *(v512sf*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov10:
**	li\s+[a-x0-9]+,1024
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]
**	vle32\.v\tv8,0\s*\([a-x0-9]+\)
**  ...
**  vmv8r\.v\tv16,v8
**	...
**  ret
*/
void mov10 (float *in, float *out) 
{ 
 register v1024sf v2 asm("v8") = *(v1024sf*)in; 
 asm volatile ("# %0"::"vr"(v2)); 
 register v1024sf v4 asm("v16") = v2; 
 *(v1024sf*)out = v4; 
 asm volatile ("# %0"::"vr"(v4)); 
}
