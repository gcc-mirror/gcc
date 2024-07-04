/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -mrvv-max-lmul=m8 -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "def.h"

/*
** mov0:
**	vsetivli\s+zero,\s*1,\s*e8,\s*mf8,\s*t[au],\s*m[au]
**	vle8\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov0 (int8_t *in, int8_t *out) 
{ 
 register v1qi v1 asm("v1") = *(v1qi*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v1qi v2 asm("v2") = v1; 
 *(v1qi*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov1:
**	vsetivli\s+zero,\s*2,\s*e8,\s*mf8,\s*t[au],\s*m[au]
**	vle8\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov1 (int8_t *in, int8_t *out) 
{ 
 register v2qi v1 asm("v1") = *(v2qi*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v2qi v2 asm("v2") = v1; 
 *(v2qi*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov2:
**	vsetivli\s+zero,\s*4,\s*e8,\s*mf8,\s*t[au],\s*m[au]
**	vle8\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov2 (int8_t *in, int8_t *out) 
{ 
 register v4qi v1 asm("v1") = *(v4qi*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v4qi v2 asm("v2") = v1; 
 *(v4qi*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov3:
**	vsetivli\s+zero,\s*8,\s*e8,\s*mf8,\s*t[au],\s*m[au]
**	vle8\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov3 (int8_t *in, int8_t *out) 
{ 
 register v8qi v1 asm("v1") = *(v8qi*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v8qi v2 asm("v2") = v1; 
 *(v8qi*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov4:
**	vsetivli\s+zero,\s*16,\s*e8,\s*mf8,\s*t[au],\s*m[au]
**	vle8\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov4 (int8_t *in, int8_t *out) 
{ 
 register v16qi v1 asm("v1") = *(v16qi*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v16qi v2 asm("v2") = v1; 
 *(v16qi*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov5:
**	li\s+[a-x0-9]+,32
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]
**	vle8\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov5 (int8_t *in, int8_t *out) 
{ 
 register v32qi v1 asm("v1") = *(v32qi*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v32qi v2 asm("v2") = v1; 
 *(v32qi*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov6:
**	li\s+[a-x0-9]+,64
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]
**	vle8\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov6 (int8_t *in, int8_t *out) 
{ 
 register v64qi v1 asm("v1") = *(v64qi*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v64qi v2 asm("v2") = v1; 
 *(v64qi*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov7:
**	li\s+[a-x0-9]+,128
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]
**	vle8\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov7 (int8_t *in, int8_t *out) 
{ 
 register v128qi v1 asm("v1") = *(v128qi*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v128qi v2 asm("v2") = v1; 
 *(v128qi*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov8:
**	li\s+[a-x0-9]+,256
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]
**	vle8\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov8 (int8_t *in, int8_t *out) 
{ 
 register v256qi v1 asm("v1") = *(v256qi*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v256qi v2 asm("v2") = v1; 
 *(v256qi*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov9:
**	li\s+[a-x0-9]+,512
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]
**	vle8\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov9 (int8_t *in, int8_t *out) 
{ 
 register v512qi v1 asm("v1") = *(v512qi*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register v512qi v2 asm("v2") = v1; 
 *(v512qi*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov10:
**	li\s+[a-x0-9]+,1024
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]
**	vle8\.v\tv2,0\s*\([a-x0-9]+\)
**  ...
**  vmv2r\.v\tv4,v2
**	...
**  ret
*/
void mov10 (uint8_t *in, uint8_t *out) 
{ 
 register v1024qi v2 asm("v2") = *(v1024qi*)in; 
 asm volatile ("# %0"::"vr"(v2)); 
 register v1024qi v4 asm("v4") = v2; 
 *(v1024qi*)out = v4; 
 asm volatile ("# %0"::"vr"(v4)); 
}

/*
** mov11:
**	li\s+[a-x0-9]+,4096
**	addi\s+[a-x0-9]+,[a-x0-9]+,-2048
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]
**	vle8\.v\tv4,0\s*\([a-x0-9]+\)
**  ...
**  vmv4r\.v\tv8,v4
**	...
**  ret
*/
void mov11 (uint8_t *in, uint8_t *out) 
{ 
 register v2048qi v4 asm("v4") = *(v2048qi*)in; 
 asm volatile ("# %0"::"vr"(v4)); 
 register v2048qi v8 asm("v8") = v4; 
 *(v2048qi*)out = v8; 
 asm volatile ("# %0"::"vr"(v8)); 
}

/*
** mov12:
**	li\s+[a-x0-9]+,4096
**	vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]
**	vle8\.v\tv8,0\s*\([a-x0-9]+\)
**  ...
**  vmv8r\.v\tv16,v8
**	...
**  ret
*/
void mov12 (uint8_t *in, uint8_t *out) 
{ 
 register v4096qi v8 asm("v8") = *(v4096qi*)in; 
 asm volatile ("# %0"::"vr"(v8)); 
 register v4096qi v16 asm("v16") = v8; 
 *(v4096qi*)out = v16; 
 asm volatile ("# %0"::"vr"(v16)); 
}
