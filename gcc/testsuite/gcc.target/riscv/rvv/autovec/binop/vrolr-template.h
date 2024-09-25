#include <stdint-gcc.h>

#define VROL_VV(SEW, S, T) \
__attribute__ ((noipa))\
void autovect_vrol_vv_##S##SEW (T *out,  T *op1, T *op2, int n){\
    for(int i=0; i<n; i++){ \
        op2[i] = op2[i] & (SEW-1);\
        out[i]= (op1[i] << op2[i]) | (op1[i] >> (SEW - op2[i]));\
    }\
}

#define VROR_VV(SEW, S, T) \
__attribute__ ((noipa))\
void autovect_vror_vv_##S##SEW (T *out,  T *op1, T *op2, int n){\
    for(int i=0; i<n; i++){ \
        op2[i] = op2[i] & (SEW-1);\
        out[i]= (op1[i] >> op2[i]) | (op1[i] << (SEW - op2[i]));\
    }\
}

VROL_VV(8,  u, uint8_t)
VROL_VV(16, u, uint16_t)
VROL_VV(32, u, uint32_t)
VROL_VV(64, u, uint64_t)

VROR_VV(8,  u, uint8_t)
VROR_VV(16, u, uint16_t)
VROR_VV(32, u, uint32_t)
VROR_VV(64, u, uint64_t)
