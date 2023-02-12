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
void mov1 (uint8_t *in, uint8_t *out) 
{ 
 register vuint8mf8_t v1 asm("v1") = *(vuint8mf8_t*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register vuint8mf8_t v2 asm("v2") = v1; 
 *(vuint8mf8_t*)out = v2; 
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
void mov2 (uint8_t *in, uint8_t *out) 
{ 
 register vuint8mf4_t v1 asm("v1") = *(vuint8mf4_t*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register vuint8mf4_t v2 asm("v2") = v1; 
 *(vuint8mf4_t*)out = v2; 
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
void mov3 (uint8_t *in, uint8_t *out) 
{ 
 register vuint8mf2_t v1 asm("v1") = *(vuint8mf2_t*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register vuint8mf2_t v2 asm("v2") = v1; 
 *(vuint8mf2_t*)out = v2; 
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
void mov4 (uint8_t *in, uint8_t *out) 
{ 
 register vuint8m1_t v1 asm("v1") = *(vuint8m1_t*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register vuint8m1_t v2 asm("v2") = v1; 
 *(vuint8m1_t*)out = v2; 
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
void mov5 (uint8_t *in, uint8_t *out) 
{ 
 register vuint8m2_t v2 asm("v2") = *(vuint8m2_t*)in; 
 asm volatile ("# %0"::"vr"(v2)); 
 register vuint8m2_t v4 asm("v4") = v2; 
 *(vuint8m2_t*)out = v4; 
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
void mov6 (uint8_t *in, uint8_t *out) 
{ 
 register vuint8m4_t v4 asm("v4") = *(vuint8m4_t*)in; 
 asm volatile ("# %0"::"vr"(v4)); 
 register vuint8m4_t v8 asm("v8") = v4; 
 *(vuint8m4_t*)out = v8; 
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
void mov7 (uint8_t *in, uint8_t *out) 
{ 
 register vuint8m8_t v8 asm("v8") = *(vuint8m8_t*)in; 
 asm volatile ("# %0"::"vr"(v8)); 
 register vuint8m8_t v16 asm("v16") = v8; 
 *(vuint8m8_t*)out = v16; 
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
void mov8 (uint16_t *in, uint16_t *out) 
{ 
 register vuint16mf4_t v1 asm("v1") = *(vuint16mf4_t*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register vuint16mf4_t v2 asm("v2") = v1; 
 *(vuint16mf4_t*)out = v2; 
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
void mov9 (uint16_t *in, uint16_t *out) 
{ 
 register vuint16mf2_t v1 asm("v1") = *(vuint16mf2_t*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register vuint16mf2_t v2 asm("v2") = v1; 
 *(vuint16mf2_t*)out = v2; 
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
void mov10 (uint16_t *in, uint16_t *out) 
{ 
 register vuint16m1_t v1 asm("v1") = *(vuint16m1_t*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register vuint16m1_t v2 asm("v2") = v1; 
 *(vuint16m1_t*)out = v2; 
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
void mov11 (uint16_t *in, uint16_t *out) 
{ 
 register vuint16m2_t v2 asm("v2") = *(vuint16m2_t*)in; 
 asm volatile ("# %0"::"vr"(v2)); 
 register vuint16m2_t v4 asm("v4") = v2; 
 *(vuint16m2_t*)out = v4; 
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
void mov12 (uint16_t *in, uint16_t *out) 
{ 
 register vuint16m4_t v4 asm("v4") = *(vuint16m4_t*)in; 
 asm volatile ("# %0"::"vr"(v4)); 
 register vuint16m4_t v8 asm("v8") = v4; 
 *(vuint16m4_t*)out = v8; 
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
void mov13 (uint32_t *in, uint32_t *out) 
{ 
 register vuint16m8_t v8 asm("v8") = *(vuint16m8_t*)in; 
 asm volatile ("# %0"::"vr"(v8)); 
 register vuint16m8_t v16 asm("v16") = v8; 
 *(vuint16m8_t*)out = v16; 
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
void mov14 (uint32_t *in, uint32_t *out) 
{ 
 register vuint32mf2_t v1 asm("v1") = *(vuint32mf2_t*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register vuint32mf2_t v2 asm("v2") = v1; 
 *(vuint32mf2_t*)out = v2; 
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
void mov15 (uint32_t *in, uint32_t *out) 
{ 
 register vuint32m1_t v1 asm("v1") = *(vuint32m1_t*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register vuint32m1_t v2 asm("v2") = v1; 
 *(vuint32m1_t*)out = v2; 
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
void mov16 (uint32_t *in, uint32_t *out) 
{ 
 register vuint32m2_t v2 asm("v2") = *(vuint32m2_t*)in; 
 asm volatile ("# %0"::"vr"(v2)); 
 register vuint32m2_t v4 asm("v4") = v2; 
 *(vuint32m2_t*)out = v4; 
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
void mov17 (uint32_t *in, uint32_t *out)
{ 
 register vuint32m4_t v4 asm("v4") = *(vuint32m4_t*)in; 
 asm volatile ("# %0"::"vr"(v4)); 
 register vuint32m4_t v8 asm("v8") = v4; 
 *(vuint32m4_t*)out = v8; 
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
void mov18 (uint32_t *in, uint32_t *out) 
{ 
 register vuint32m8_t v8 asm("v8") = *(vuint32m8_t*)in; 
 asm volatile ("# %0"::"vr"(v8)); 
 register vuint32m8_t v16 asm("v16") = v8; 
 *(vuint32m8_t*)out = v16; 
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
 register vuint64m1_t v1 asm("v1") = *(vuint64m1_t*)in; 
 asm volatile ("# %0"::"vr"(v1)); 
 register vuint64m1_t v2 asm("v2") = v1; 
 *(vuint64m1_t*)out = v2; 
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
 register vuint64m2_t v2 asm("v2") = *(vuint64m2_t*)in; 
 asm volatile ("# %0"::"vr"(v2)); 
 register vuint64m2_t v4 asm("v4") = v2; 
 *(vuint64m2_t*)out = v4; 
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
 register vuint64m4_t v4 asm("v4") = *(vuint64m4_t*)in; 
 asm volatile ("# %0"::"vr"(v4)); 
 register vuint64m4_t v8 asm("v8") = v4; 
 *(vuint64m4_t*)out = v8; 
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
 register vuint64m8_t v8 asm("v8") = *(vuint64m8_t*)in; 
 asm volatile ("# %0"::"vr"(v8)); 
 register vuint64m8_t v16 asm("v16") = v8; 
 *(vuint64m8_t*)out = v16; 
 asm volatile ("# %0"::"vr"(v16)); 
}
