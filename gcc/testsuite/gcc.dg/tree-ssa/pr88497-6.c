/* { dg-do compile } */
/* { dg-require-effective-target avx512f } */
/* { dg-options "-O2 -mavx512f -ffast-math -fdump-tree-reassoc1" } */

/* To test reassoc can undistribute vector bit_field_ref on multiple
   vector machine modes.

   v1, v2 of type vector 4 x float
   v3, v4 of type vector 8 x float
   v5, v6 of type vector 16 x float

   reassoc transforms

     accumulator  +=  v1[0]  + v1[1]  + v1[2]  + v1[3]  +
                      v2[0]  + v2[1]  + v2[2]  + v2[3]  +
                      v3[0]  + v3[1]  + v3[2]  + v3[3]  +
                      v3[4]  + v3[5]  + v3[6]  + v3[7]  +
                      v4[0]  + v4[1]  + v4[2]  + v4[3]  +
                      v4[4]  + v4[5]  + v4[6]  + v4[7]  +
                      v5[0]  + v5[1]  + v5[2]  + v5[3]  +
                      v5[4]  + v5[5]  + v5[6]  + v5[7]  +
                      v5[8]  + v5[9]  + v5[10] + v5[11] +
                      v5[12] + v5[13] + v5[14] + v5[15] +
                      v6[0]  + v6[1]  + v6[2]  + v6[3]  +
                      v6[4]  + v6[5]  + v6[6]  + v6[7]  +
                      v6[8]  + v6[9]  + v6[10] + v6[11] +
                      v6[12] + v6[13] + v6[14] + v6[15] ;

   into:

     T12 = v1 + v2;
     T34 = v3 + v4;
     T56 = v5 + v6;
     accumulator += T12[0]  + T12[1]  + T12[2]  + T12[3]  +
     accumulator += T34[0]  + T34[1]  + T34[2]  + T34[3]  +
     accumulator += T34[4]  + T34[5]  + T34[6]  + T34[7]  +
     accumulator += T56[0]  + T56[1]  + T56[2]  + T56[3]  +
     accumulator += T56[4]  + T56[5]  + T56[6]  + T56[7]  +
     accumulator += T56[8]  + T56[9]  + T56[10] + T56[11] +
     accumulator += T56[12] + T56[13] + T56[14] + T56[15] ;  */

typedef float v4sf __attribute__((vector_size(16)));
typedef float v8sf __attribute__((vector_size(32)));
typedef float v16sf __attribute__((vector_size(64)));

float
test (float accumulator, v4sf v1, v4sf v2, v8sf v3, v8sf v4, v16sf v5, v16sf v6)
{
  accumulator += v1[0] + v1[1] + v1[2] + v1[3];
  accumulator += v2[0] + v2[1] + v2[2] + v2[3];
  accumulator += v3[0] + v3[1] + v3[2] + v3[3];
  accumulator += v3[4] + v3[5] + v3[6] + v3[7];
  accumulator += v4[0] + v4[1] + v4[2] + v4[3];
  accumulator += v4[4] + v4[5] + v4[6] + v4[7];
  accumulator += v5[0] + v5[1] + v5[2] + v5[3];
  accumulator += v5[4] + v5[5] + v5[6] + v5[7];
  accumulator += v5[8] + v5[9] + v5[10] + v5[11];
  accumulator += v5[12] + v5[13] + v5[14] + v5[15];
  accumulator += v6[0] + v6[1] + v6[2] + v6[3];
  accumulator += v6[4] + v6[5] + v6[6] + v6[7];
  accumulator += v6[8] + v6[9] + v6[10] + v6[11];
  accumulator += v6[12] + v6[13] + v6[14] + v6[15];
  return accumulator;
}
/* { dg-final { scan-tree-dump-times "BIT_FIELD_REF" 28 "reassoc1" } } */
