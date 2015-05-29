/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */


void vector_fmul_reverse_c(float *dst, const float *src0, const float *src1,
int len){
    int i;
    src1 += len-1;
    for(i=0; i<len; i++)
        dst[i] = src0[i] * src1[-i];
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_perm && vect_hw_misalign } } } } */
