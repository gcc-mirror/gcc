/* { dg-require-effective-target avx512f_runtime } */
/* { dg-additional-options "-O2 -mavx512f -ffast-math -fdump-tree-reassoc1" } */

/* To test reassoc can undistribute vector bit_field_ref on multiple
   vector machine modes, bypass those modes with only one candidate.

   v1, v2 of type vector 4 x float
   v3     of type vector 8 x float
   v5, v6 of type vector 16 x float

   reassoc transforms

     accumulator  +=  v1[0]  + v1[1]  + v1[2]  + v1[3]  +
                      v2[0]  + v2[1]  + v2[2]  + v2[3]  +
                      v3[0]  + v3[1]  + v3[2]  + v3[3]  +
                      v3[4]  + v3[5]  + v3[6]  + v3[7]  +
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
     T56 = v5 + v6;
     accumulator += T12[0]  + T12[1]  + T12[2]  + T12[3]  +
     accumulator += v3[0]   + v3[1]   + v3[2]   + v3[3]   +
     accumulator += v3[4]   + v3[5]   + v3[6]   + v3[7]   +
     accumulator += T56[0]  + T56[1]  + T56[2]  + T56[3]  +
     accumulator += T56[4]  + T56[5]  + T56[6]  + T56[7]  +
     accumulator += T56[8]  + T56[9]  + T56[10] + T56[11] +
     accumulator += T56[12] + T56[13] + T56[14] + T56[15] ;  */

typedef float v4sf __attribute__((vector_size(16)));
typedef float v8sf __attribute__((vector_size(32)));
typedef float v16sf __attribute__((vector_size(64)));

__attribute__ ((noinline))
float test(float accumulator, v4sf v1, v4sf v2, v8sf v3, v16sf v5, v16sf v6) {
  accumulator += v1[0] + v1[1] + v1[2] + v1[3];
  accumulator += v2[0] + v2[1] + v2[2] + v2[3];
  accumulator += v3[0] + v3[1] + v3[2] + v3[3];
  accumulator += v3[4] + v3[5] + v3[6] + v3[7];
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

extern void abort (void);

int
main ()
{
  v4sf v1 = {1.0, 2.0, 3.0, 4.0 };
  v4sf v2 = {5.0, 6.0, 7.0, 8.0 };
  v8sf v3 = {9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0 };
  v16sf v5 = {17.0, 18.0, 19.0, 20.0, 21.0, 22.0, 23.0, 24.0, 25.0, 26.0, 27.0, 28.0, 29.0, 30.0, 31.0, 32.0};
  v16sf v6 = {33.0, 34.0, 35.0, 36.0, 37.0, 38.0, 39.0, 40.0, 41.0, 42.0, 43.0, 44.0, 45.0, 46.0, 47.0, 48.0};
  float acc = 24.0;
  double res = test (acc, v1, v2, v3, v5, v6);
  if (res != 1200.0)
    abort();
  return 0;
}

/* { dg-final { scan-tree-dump-times "BIT_FIELD_REF" 28 "reassoc1" } } */
