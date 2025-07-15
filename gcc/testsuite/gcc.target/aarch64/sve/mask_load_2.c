// { dg-do compile }
// { dg-options "-march=armv8-a+sve -msve-vector-bits=128 -O3" }

typedef struct Array {
    int elems[3];
} Array;

int loop(Array **pp, int len, int idx) {
    int nRet = 0;

    #pragma GCC unroll 0
    for (int i = 0; i < len; i++) {
        Array *p = pp[i];
        if (p) {
            nRet += p->elems[idx];
        }
    }

    return nRet;
}

// { dg-final { scan-assembler-times {ld1w\tz[0-9]+\.d, p[0-7]/z} 1 } }
// { dg-final { scan-assembler-times {add\tz[0-9]+\.s, p[0-7]/m}  1 } }
