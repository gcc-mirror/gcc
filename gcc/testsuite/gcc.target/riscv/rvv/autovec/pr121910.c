/* { dg-do compile } */
/* { dg-options "-O3 -ftree-vectorize -mabi=lp64d -march=rv64gcv -mrvv-max-lmul=dynamic -fdump-tree-vect-all" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O2" "-Os" "-Og" "-Oz" } } */

#include <stdint-gcc.h>

/* full chroma mc (ie until 1/8 pixel)*/
void mc_chroma(uint8_t* dst, int i_dst_stride, uint8_t* src, int i_src_stride,
               int mvx, int mvy, int i_width, int i_height) {
    uint8_t* srcp;

    int d8x = mvx & 0x07;
    int d8y = mvy & 0x07;
    int cA = (8 - d8x) * (8 - d8y);
    int cB = d8x * (8 - d8y);
    int cC = (8 - d8x) * d8y;
    int cD = d8x * d8y;

    src += (mvy >> 3) * i_src_stride + (mvx >> 3);
    srcp = &src[i_src_stride];

    for (int y = 0; y < i_height; y++) {
        for (int x = 0; x < i_width; x++)
            dst[x] = (cA * src[x] + cB * src[x + 1] + cC * srcp[x] +
                      cD * srcp[x + 1] + 32) >>
                     6;
        dst += i_dst_stride;
        src = srcp;
        srcp += i_src_stride;
    }
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
/* { dg-final { scan-tree-dump "Choosing vector mode RVVM1QI" "vect" } } */
