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
void mov0 (int64_t *in, int64_t *out) 
{ 
 register v1di v1 asm("v1") = *(v1di*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v1di v2 asm("v2") = v1; 
 *(v1di*)out = v2; 
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
void mov1 (int64_t *in, int64_t *out) 
{ 
 register v2di v1 asm("v1") = *(v2di*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v2di v2 asm("v2") = v1; 
 *(v2di*)out = v2; 
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
void mov2 (int64_t *in, int64_t *out) 
{ 
 register v4di v1 asm("v1") = *(v4di*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v4di v2 asm("v2") = v1; 
 *(v4di*)out = v2; 
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
void mov3 (int64_t *in, int64_t *out) 
{ 
 register v8di v1 asm("v1") = *(v8di*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v8di v2 asm("v2") = v1; 
 *(v8di*)out = v2; 
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
void mov4 (int64_t *in, int64_t *out) 
{ 
 register v16di v1 asm("v1") = *(v16di*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v16di v2 asm("v2") = v1; 
 *(v16di*)out = v2; 
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
void mov5 (int64_t *in, int64_t *out) 
{ 
 register v32di v1 asm("v1") = *(v32di*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v32di v2 asm("v2") = v1; 
 *(v32di*)out = v2; 
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
void mov6 (int64_t *in, int64_t *out) 
{ 
 register v64di v1 asm("v1") = *(v64di*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v64di v2 asm("v2") = v1; 
 *(v64di*)out = v2; 
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
void mov7 (int64_t *in, int64_t *out) 
{ 
 register v128di v1 asm("v2") = *(v128di*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v128di v2 asm("v4") = v1; 
 *(v128di*)out = v2; 
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
void mov8 (int64_t *in, int64_t *out) 
{ 
 register v256di v1 asm("v4") = *(v256di*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v256di v2 asm("v8") = v1; 
 *(v256di*)out = v2; 
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
void mov9 (int64_t *in, int64_t *out) 
{ 
 register v512di v1 asm("v8") = *(v512di*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v512di v2 asm("v16") = v1; 
 *(v512di*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}
