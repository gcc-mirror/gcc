/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -mrvv-max-lmul=m8 -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "def.h"

/*
** mov0:
**	vsetivli\s+zero,\s*1,\s*e64,\s*m1,\s*t[au],\s*m[au]
**	vle64\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov0 (double *in, double *out) 
{ 
 register v1df v1 asm("v1") = *(v1df*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v1df v2 asm("v2") = v1; 
 *(v1df*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov1:
**	vsetivli\s+zero,\s*2,\s*e64,\s*m1,\s*t[au],\s*m[au]
**	vle64\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov1 (double *in, double *out) 
{ 
 register v2df v1 asm("v1") = *(v2df*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v2df v2 asm("v2") = v1; 
 *(v2df*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov2:
**	vsetivli\s+zero,\s*4,\s*e64,\s*m1,\s*t[au],\s*m[au]
**	vle64\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov2 (double *in, double *out) 
{ 
 register v4df v1 asm("v1") = *(v4df*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v4df v2 asm("v2") = v1; 
 *(v4df*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov3:
**	vsetivli\s+zero,\s*8,\s*e64,\s*m1,\s*t[au],\s*m[au]
**	vle64\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov3 (double *in, double *out) 
{ 
 register v8df v1 asm("v1") = *(v8df*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v8df v2 asm("v2") = v1; 
 *(v8df*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov4:
**	vsetivli\s+zero,\s*16,\s*e64,\s*m1,\s*t[au],\s*m[au]
**	vle64\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov4 (double *in, double *out) 
{ 
 register v16df v1 asm("v1") = *(v16df*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v16df v2 asm("v2") = v1; 
 *(v16df*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov5:
**	li\s+[a-x0-9]+,32
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]
**	vle64\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov5 (double *in, double *out) 
{ 
 register v32df v1 asm("v1") = *(v32df*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v32df v2 asm("v2") = v1; 
 *(v32df*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov6:
**	li\s+[a-x0-9]+,64
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]
**	vle64\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov6 (double *in, double *out) 
{ 
 register v64df v1 asm("v1") = *(v64df*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v64df v2 asm("v2") = v1; 
 *(v64df*)out = v2; 
 asm volatile ("# %0"::"vr"(v2));
}

/*
** mov7:
**	li\s+[a-x0-9]+,128
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]
**	vle64\.v\tv2,0\s*\([a-x0-9]+\)
**  ...
**  vmv2r\.v\tv4,v2
**	...
**  ret
*/
void mov7 (double *in, double *out) 
{ 
 register v128df v1 asm("v2") = *(v128df*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v128df v2 asm("v4") = v1; 
 *(v128df*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov8:
**	li\s+[a-x0-9]+,256
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]
**	vle64\.v\tv4,0\s*\([a-x0-9]+\)
**  ...
**  vmv4r\.v\tv8,v4
**	...
**  ret
*/
void mov8 (double *in, double *out) 
{ 
 register v256df v1 asm("v4") = *(v256df*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v256df v2 asm("v8") = v1; 
 *(v256df*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov9:
**	li\s+[a-x0-9]+,512
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]
**	vle64\.v\tv8,0\s*\([a-x0-9]+\)
**  ...
**  vmv8r\.v\tv16,v8
**	...
**  ret
*/
void mov9 (double *in, double *out) 
{ 
 register v512df v1 asm("v8") = *(v512df*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v512df v2 asm("v16") = v1; 
 *(v512df*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}
