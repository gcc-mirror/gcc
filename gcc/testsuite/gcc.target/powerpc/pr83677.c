/* { dg-do run { target { powerpc*-*-* && { lp64 && p9vector_hw } } } } */
/* { dg-options "-mdejagnu-cpu=power9 -O2 " } */

/* PR83677: This test case used to fail due to mis-generation of the
   xxpermr instruction.  It requires inlining to create enough register
   pressure that we generate xxpermr rather than vpermr.  */

#include <altivec.h>

void v_expand_u8(vector unsigned char* a, vector unsigned short* b0, vector unsigned short* b1)
{
  *b0 = (vector unsigned short)vec_mergeh(*a, vec_splats((unsigned char)0));
  *b1 = (vector unsigned short)vec_mergel(*a, vec_splats((unsigned char)0));
}

void v_expand_u16(vector unsigned short* a, vector unsigned int* b0, vector unsigned int* b1)
{
    *b0 = (vector unsigned int)vec_mergeh(*a, vec_splats((unsigned short)0));
    *b1 = (vector unsigned int)vec_mergel(*a, vec_splats((unsigned short)0));
}

void v_load_deinterleave_u8(unsigned char *ptr, vector unsigned char* a, vector unsigned char* b, vector unsigned char* c)
{
    vector unsigned char v1 = vec_xl( 0, ptr);
    vector unsigned char v2 = vec_xl(16, ptr);
    vector unsigned char v3 = vec_xl(32, ptr);

    static const vector unsigned char a12_perm = {0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 0, 0, 0, 0, 0};
    static const vector unsigned char a123_perm = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 17, 20, 23, 26, 29};
    *a = vec_perm(vec_perm(v1, v2, a12_perm), v3, a123_perm);

    static const vector unsigned char b12_perm = {1, 4, 7, 10, 13, 16, 19, 22, 25, 28, 31, 0, 0, 0, 0, 0};
    static const vector unsigned char b123_perm = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 18, 21, 24, 27, 30};
    *b = vec_perm(vec_perm(v1, v2, b12_perm), v3, b123_perm);

    static const vector unsigned char c12_perm = {2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 0, 0, 0, 0, 0, 0};
    static const vector unsigned char c123_perm = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 16, 19, 22, 25, 28, 31};
    *c = vec_perm(vec_perm(v1, v2, c12_perm), v3, c123_perm);
}

void v_load_deinterleave_f32(float *ptr, vector float* a, vector float* b, vector float* c)
{
    vector float v1 = vec_xl( 0, ptr);
    vector float v2 = vec_xl(16, ptr);
    vector float v3 = vec_xl(32, ptr);

    static const vector unsigned char flp = {0, 1, 2, 3, 12, 13, 14, 15, 16, 17, 18, 19, 28, 29, 30, 31};
    *a = vec_perm(v1, vec_sld(v3, v2, 8), flp);

    static const vector unsigned char flp2 = {28, 29, 30, 31, 0, 1, 2, 3, 12, 13, 14, 15, 16, 17, 18, 19};
    *b = vec_perm(v2, vec_sld(v1, v3, 8), flp2);

    *c = vec_perm(vec_sld(v2, v1, 8), v3, flp);
}

void v_store_interleave_f32(float *ptr, vector float a, vector float b, vector float c)
{
    vector float hbc = vec_mergeh(b, c);

    static const vector unsigned char ahbc = {0, 1, 2, 3, 16, 17, 18, 19, 20, 21, 22, 23, 4, 5, 6, 7};
    vec_xst(vec_perm(a, hbc, ahbc),  0, ptr);

    vector float lab = vec_mergel(a, b);
    vec_xst(vec_sld(lab, hbc, 8), 16, ptr);

    static const vector unsigned char clab = {8, 9, 10, 11, 24, 25, 26, 27, 28, 29, 30, 31, 12, 13, 14, 15};
    vec_xst(vec_perm(c, lab, clab), 32, ptr);
}

vector float v_cvt_f32(vector unsigned int a)
{
    return (vector float)vec_ctf(a, 0);
}

void acc_simd_(const unsigned char* src, float* dst, const unsigned char* mask, int len)
{
    int x = 0;
    const int cVectorWidth = 16;

            for ( ; x <= len - cVectorWidth; x += cVectorWidth)
            {
                vector unsigned char v_mask = vec_xl(0, mask + x);
                v_mask = (vector unsigned char)vec_cmpeq(vec_splats((unsigned char)0), v_mask);
                v_mask = (vector unsigned char)vec_nor(v_mask, v_mask);
                vector unsigned char v_src0, v_src1, v_src2;
                v_load_deinterleave_u8((unsigned char *)(src + (x * 3)), &v_src0, &v_src1, &v_src2);
                v_src0 = v_src0 & v_mask;
                v_src1 = v_src1 & v_mask;
                v_src2 = v_src2 & v_mask;

                /* expand 16 uchar to 4 vectors which contains 4 uint */
                vector unsigned short v_src00, v_src01, v_src10, v_src11, v_src20, v_src21;
                v_expand_u8(&v_src0, &v_src00, &v_src01);
                v_expand_u8(&v_src1, &v_src10, &v_src11);
                v_expand_u8(&v_src2, &v_src20, &v_src21);
                vector unsigned int v_src000, v_src001, v_src010, v_src011;
                vector unsigned int v_src100, v_src101, v_src110, v_src111;
                vector unsigned int v_src200, v_src201, v_src210, v_src211;
                v_expand_u16(&v_src00, &v_src000, &v_src001);
                v_expand_u16(&v_src01, &v_src010, &v_src011);
                v_expand_u16(&v_src10, &v_src100, &v_src101);
                v_expand_u16(&v_src11, &v_src110, &v_src111);
                v_expand_u16(&v_src20, &v_src200, &v_src201);
                v_expand_u16(&v_src21, &v_src210, &v_src211);

                vector float v_dst000, v_dst001, v_dst010, v_dst011;
                vector float v_dst100, v_dst101, v_dst110, v_dst111;
                vector float v_dst200, v_dst201, v_dst210, v_dst211;
                v_load_deinterleave_f32(dst + (x * 3),        &v_dst000, &v_dst100, &v_dst200);
                v_load_deinterleave_f32(dst + ((x + 4) * 3),  &v_dst001, &v_dst101, &v_dst201);
                v_load_deinterleave_f32(dst + ((x + 8) * 3),  &v_dst010, &v_dst110, &v_dst210);
                v_load_deinterleave_f32(dst + ((x + 12) * 3), &v_dst011, &v_dst111, &v_dst211);

                v_store_interleave_f32(dst + (x * 3),        vec_add(v_dst000, v_cvt_f32(v_src000)), vec_add(v_dst100, v_cvt_f32(v_src100)), vec_add(v_dst200, v_cvt_f32(v_src200)));
                v_store_interleave_f32(dst + ((x + 4) * 3),  vec_add(v_dst001, v_cvt_f32(v_src001)), vec_add(v_dst101, v_cvt_f32(v_src101)), vec_add(v_dst201, v_cvt_f32(v_src201)));
                v_store_interleave_f32(dst + ((x + 8) * 3),  vec_add(v_dst010, v_cvt_f32(v_src010)), vec_add(v_dst110, v_cvt_f32(v_src110)), vec_add(v_dst210, v_cvt_f32(v_src210)));
                v_store_interleave_f32(dst + ((x + 12) * 3), vec_add(v_dst011, v_cvt_f32(v_src011)), vec_add(v_dst111, v_cvt_f32(v_src111)), vec_add(v_dst211, v_cvt_f32(v_src211)));
            }
    return;
}

void acc_(const unsigned char* src, float* dst, const unsigned char* mask, int len)
{
    int x = 0;
    src += (x * 3);
    dst += (x * 3);
    for( ; x < len; x++, src += 3, dst += 3 )
    {
        if( mask[x] ) /* if mask, R/G/B dst[] += src[] */
        {
            for( int k = 0; k < 3; k++ )
            {
                dst[k] += src[k];
            }
        }
    }
    return;
}

#define N 16

int main(int argc, char *argv[])
{
    unsigned char __attribute__ ((aligned (16) )) mask[] = {0, 0, 0, 0,  0, 0, 0, 0,  0, 1, 0, 0,  1, 0, 0, 1};
    unsigned char __attribute__ ((aligned (16) )) src[3*N];
    float __attribute__ ((aligned (16) )) dst[3*N];
    float __attribute__ ((aligned (16) )) exp[3*N];

    int i;

    /* initialize src and dst */
    for (i=0; i<3*N; i++) src[i] = (unsigned char)(i*3);
    for (i=0; i<3*N; i++) {dst[i] = i * 1.0f; exp[i] = dst[i];}

    acc_(src, exp, mask, N);
    acc_simd_(src, dst, mask, N);

    for (i=0; i<N; i++)
    {
        if ((dst[3*i] != exp[3*i]) || (dst[3*i+1] != exp[3*i+1]) || (dst[3*i+2] != exp[3*i+2]))
	  __builtin_abort ();
    }

    return 0;
}
