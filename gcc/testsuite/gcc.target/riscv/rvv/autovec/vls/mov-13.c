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
void mov0 (int32_t *in, int32_t *out) 
{ 
 register v1si v1 asm("v1") = *(v1si*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v1si v2 asm("v2") = v1; 
 *(v1si*)out = v2; 
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
void mov1 (int32_t *in, int32_t *out) 
{ 
 register v2si v1 asm("v1") = *(v2si*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v2si v2 asm("v2") = v1; 
 *(v2si*)out = v2; 
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
void mov2 (int32_t *in, int32_t *out) 
{ 
 register v4si v1 asm("v1") = *(v4si*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v4si v2 asm("v2") = v1; 
 *(v4si*)out = v2; 
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
void mov3 (int32_t *in, int32_t *out) 
{ 
 register v8si v1 asm("v1") = *(v8si*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v8si v2 asm("v2") = v1; 
 *(v8si*)out = v2; 
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
void mov4 (int32_t *in, int32_t *out) 
{ 
 register v16si v1 asm("v1") = *(v16si*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v16si v2 asm("v2") = v1; 
 *(v16si*)out = v2; 
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
void mov5 (int32_t *in, int32_t *out) 
{ 
 register v32si v1 asm("v1") = *(v32si*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v32si v2 asm("v2") = v1; 
 *(v32si*)out = v2; 
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
void mov6 (int32_t *in, int32_t *out) 
{ 
 register v64si v1 asm("v1") = *(v64si*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v64si v2 asm("v2") = v1; 
 *(v64si*)out = v2; 
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
void mov7 (int32_t *in, int32_t *out) 
{ 
 register v128si v1 asm("v1") = *(v128si*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v128si v2 asm("v2") = v1; 
 *(v128si*)out = v2; 
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
void mov8 (int32_t *in, int32_t *out) 
{ 
 register v256si v1 asm("v2") = *(v256si*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v256si v2 asm("v4") = v1; 
 *(v256si*)out = v2; 
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
void mov9 (int32_t *in, int32_t *out) 
{ 
 register v512si v1 asm("v4") = *(v512si*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v512si v2 asm("v8") = v1; 
 *(v512si*)out = v2; 
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
void mov10 (int32_t *in, int32_t *out) 
{ 
 register v1024si v2 asm("v8") = *(v1024si*)in; 
 asm volatile ("# %0"::"vr"(v2)); 
 register v1024si v4 asm("v16") = v2; 
 *(v1024si*)out = v4; 
 asm volatile ("# %0"::"vr"(v4)); 
}
