/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h" 

/*
** mov1:
**	vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]
**	vle8\.v\tv1,0\s*\([a-x0-9]+\)
**  ...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov1 (int8_t *in, int8_t *out) 
{ 
 register vint8mf8_t v1 asm("v1") = *(vint8mf8_t*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register vint8mf8_t v2 asm("v2") = v1; 
 *(vint8mf8_t*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov2:
**	vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf4,\s*t[au],\s*m[au]
**	vle8\.v\tv1,0\s*\([a-x0-9]+\)
**	...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov2 (int8_t *in, int8_t *out) 
{ 
 register vint8mf4_t v1 asm("v1") = *(vint8mf4_t*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register vint8mf4_t v2 asm("v2") = v1; 
 *(vint8mf4_t*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov3:
**	vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf2,\s*t[au],\s*m[au]
**	vle8\.v\tv1,0\s*\([a-x0-9]+\)
**	...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov3 (int8_t *in, int8_t *out) 
{ 
 register vint8mf2_t v1 asm("v1") = *(vint8mf2_t*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register vint8mf2_t v2 asm("v2") = v1; 
 *(vint8mf2_t*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov4:
**	vl1re8\.v\tv1,0\s*\([a-x0-9]+\)
**	...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov4 (int8_t *in, int8_t *out) 
{ 
 register vint8m1_t v1 asm("v1") = *(vint8m1_t*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register vint8m1_t v2 asm("v2") = v1; 
 *(vint8m1_t*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov5:
**	vl2re8\.v\tv2,0\s*\([a-x0-9]+\)
**	...
**  vmv2r\.v\tv4,v2
**	...
**  ret
*/
void mov5 (int8_t *in, int8_t *out) 
{ 
 register vint8m2_t v2 asm("v2") = *(vint8m2_t*)in; 
 asm volatile ("# %0"::"vr"(v2)); 
 register vint8m2_t v4 asm("v4") = v2; 
 *(vint8m2_t*)out = v4; 
 asm volatile ("# %0"::"vr"(v4)); 
}

/*
** mov6:
**	vl4re8\.v\tv4,0\s*\([a-x0-9]+\)
**	...
**  vmv4r\.v\tv8,v4
**	...
**  ret
*/
void mov6 (int8_t *in, int8_t *out) 
{ 
 register vint8m4_t v4 asm("v4") = *(vint8m4_t*)in; 
 asm volatile ("# %0"::"vr"(v4)); 
 register vint8m4_t v8 asm("v8") = v4; 
 *(vint8m4_t*)out = v8; 
 asm volatile ("# %0"::"vr"(v8)); 
}

/*
** mov7:
**	vl8re8\.v\tv8,0\s*\([a-x0-9]+\)
**	...
**  vmv8r\.v\tv16,v8
**	...
**  ret
*/
void mov7 (int8_t *in, int8_t *out) 
{ 
 register vint8m8_t v8 asm("v8") = *(vint8m8_t*)in; 
 asm volatile ("# %0"::"vr"(v8)); 
 register vint8m8_t v16 asm("v16") = v8; 
 *(vint8m8_t*)out = v16; 
 asm volatile ("# %0"::"vr"(v16)); 
}

/*
** mov8:
**	vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf4,\s*t[au],\s*m[au]
**	vle16\.v\tv1,0\s*\([a-x0-9]+\)
**	...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov8 (int16_t *in, int16_t *out) 
{ 
 register vint16mf4_t v1 asm("v1") = *(vint16mf4_t*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register vint16mf4_t v2 asm("v2") = v1; 
 *(vint16mf4_t*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov9:
**	vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf2,\s*t[au],\s*m[au]
**	vle16\.v\tv1,0\s*\([a-x0-9]+\)
**	...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov9 (int16_t *in, int16_t *out) 
{ 
 register vint16mf2_t v1 asm("v1") = *(vint16mf2_t*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register vint16mf2_t v2 asm("v2") = v1; 
 *(vint16mf2_t*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov10:
**	vl1re16\.v\tv1,0\s*\([a-x0-9]+\)
**	...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov10 (int16_t *in, int16_t *out) 
{ 
 register vint16m1_t v1 asm("v1") = *(vint16m1_t*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register vint16m1_t v2 asm("v2") = v1; 
 *(vint16m1_t*)out = v2; 
 asm volatile ("# %0"::"vr"(v2)); 
}

/*
** mov11:
**	vl2re16\.v\tv2,0\s*\([a-x0-9]+\)
**	...
**  vmv2r\.v\tv4,v2
**	...
**  ret
*/
void mov11 (int16_t *in, int16_t *out) 
{ 
 register vint16m2_t v2 asm("v2") = *(vint16m2_t*)in; 
 asm volatile ("# %0"::"vr"(v2)); 
 register vint16m2_t v4 asm("v4") = v2; 
 *(vint16m2_t*)out = v4; 
 asm volatile ("# %0"::"vr"(v4)); 
}

/*
** mov12:
**	vl4re16\.v\tv4,0\s*\([a-x0-9]+\)
**	...
**  vmv4r\.v\tv8,v4
**	...
**  ret
*/
void mov12 (int16_t *in, int16_t *out) 
{ 
 register vint16m4_t v4 asm("v4") = *(vint16m4_t*)in; 
 asm volatile ("# %0"::"vr"(v4)); 
 register vint16m4_t v8 asm("v8") = v4; 
 *(vint16m4_t*)out = v8; 
 asm volatile ("# %0"::"vr"(v8)); 
}

/*
** mov13:
**	vl8re16\.v\tv8,0\s*\([a-x0-9]+\)
**	...
**  vmv8r\.v\tv16,v8
**	...
**  ret
*/
void mov13 (int32_t *in, int32_t *out) 
{ 
 register vint16m8_t v8 asm("v8") = *(vint16m8_t*)in; 
 asm volatile ("# %0"::"vr"(v8)); 
 register vint16m8_t v16 asm("v16") = v8; 
 *(vint16m8_t*)out = v16; 
 asm volatile ("# %0"::"vr"(v16)); 
}

/*
** mov14:
**	vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]
**	vle32\.v\tv1,0\s*\([a-x0-9]+\)
**	...
**  vmv1r\.v\tv2,v1
**	...
**  ret
*/
void mov14 (int32_t *in, int32_t *out) 
{ 
 register vint32mf2_t v1 asm("v1") = *(vint32mf2_t*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register vint32mf2_t v2 asm("v2") = v1; 
 *(vint32mf2_t*)out = v2; 
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
void mov15 (int32_t *in, int32_t *out) 
{ 
 register vint32m1_t v1 asm("v1") = *(vint32m1_t*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register vint32m1_t v2 asm("v2") = v1; 
 *(vint32m1_t*)out = v2; 
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
void mov16 (int32_t *in, int32_t *out) 
{ 
 register vint32m2_t v2 asm("v2") = *(vint32m2_t*)in; 
 asm volatile ("# %0"::"vr"(v2)); 
 register vint32m2_t v4 asm("v4") = v2; 
 *(vint32m2_t*)out = v4; 
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
void mov17 (int32_t *in, int32_t *out) 
{ 
 register vint32m4_t v4 asm("v4") = *(vint32m4_t*)in; 
 asm volatile ("# %0"::"vr"(v4)); 
 register vint32m4_t v8 asm("v8") = v4; 
 *(vint32m4_t*)out = v8; 
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
void mov18 (int32_t *in, int32_t *out) 
{ 
 register vint32m8_t v8 asm("v8") = *(vint32m8_t*)in; 
 asm volatile ("# %0"::"vr"(v8)); 
 register vint32m8_t v16 asm("v16") = v8; 
 *(vint32m8_t*)out = v16; 
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
void mov19 (int64_t *in, int64_t *out) 
{ 
 register vint64m1_t v1 asm("v1") = *(vint64m1_t*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register vint64m1_t v2 asm("v2") = v1; 
 *(vint64m1_t*)out = v2; 
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
void mov20 (int64_t *in, int64_t *out) 
{ 
 register vint64m2_t v2 asm("v2") = *(vint64m2_t*)in; 
 asm volatile ("# %0"::"vr"(v2)); 
 register vint64m2_t v4 asm("v4") = v2; 
 *(vint64m2_t*)out = v4; 
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
void mov21 (int64_t *in, int64_t *out) 
{ 
 register vint64m4_t v4 asm("v4") = *(vint64m4_t*)in; 
 asm volatile ("# %0"::"vr"(v4)); 
 register vint64m4_t v8 asm("v8") = v4; 
 *(vint64m4_t*)out = v8; 
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
void mov22 (int64_t *in, int64_t *out) 
{ 
 register vint64m8_t v8 asm("v8") = *(vint64m8_t*)in; 
 asm volatile ("# %0"::"vr"(v8)); 
 register vint64m8_t v16 asm("v16") = v8; 
 *(vint64m8_t*)out = v16; 
 asm volatile ("# %0"::"vr"(v16)); 
}
