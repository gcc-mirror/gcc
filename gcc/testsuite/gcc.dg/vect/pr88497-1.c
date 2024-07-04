/* { dg-require-effective-target vect_double } */
/* { dg-require-effective-target vsx_hw { target { powerpc*-*-* } } } */
/* { dg-require-effective-target sse2_runtime { target { i?86-*-* x86_64-*-* } } } */
/* { dg-additional-options "-O2 -ffast-math -fdump-tree-reassoc1" } */
/* { dg-additional-options "-mvsx" { target { powerpc*-*-* } } } */
/* { dg-additional-options "-msse2" { target { i?86-*-* x86_64-*-* } } } */

/* To test reassoc can undistribute vector bit_field_ref summation.

   arg1 and arg2 are two arrays whose elements of type vector double.
   Assuming:
     A0 = arg1[0], A1 = arg1[1], A2 = arg1[2], A3 = arg1[3],
     B0 = arg2[0], B1 = arg2[1], B2 = arg2[2], B3 = arg2[3],

   Then:
     V0 = A0 * B0, V1 = A1 * B1, V2 = A2 * B2, V3 = A3 * B3,

   reassoc transforms

     accumulator += V0[0] + V0[1] + V1[0] + V1[1] + V2[0] + V2[1]
          + V3[0] + V3[1];

   into:

     T = V0 + V1 + V2 + V3
     accumulator += T[0] + T[1];

   Fewer bit_field_refs, only two for 128 or more bits vector.  */

typedef double v2df __attribute__ ((vector_size (16)));
__attribute__ ((noinline)) double
test (double accumulator, v2df arg1[], v2df arg2[])
{
  v2df temp;
  temp = arg1[0] * arg2[0];
  accumulator += temp[0] + temp[1];
  temp = arg1[1] * arg2[1];
  accumulator += temp[0] + temp[1];
  temp = arg1[2] * arg2[2];
  accumulator += temp[0] + temp[1];
  temp = arg1[3] * arg2[3];
  accumulator += temp[0] + temp[1];
  return accumulator;
}

extern void abort (void);

int
main ()
{
  v2df v2[4] = {{1.0, 2.0}, {4.0, 8.0}, {1.0, 3.0}, {9.0, 27.0}};
  v2df v3[4] = {{1.0, 4.0}, {16.0, 64.0}, {1.0, 2.0}, {3.0, 4.0}};
  double acc = 100.0;
  double res = test (acc, v2, v3);
  if (res != 827.0)
    abort ();
  return 0;
}
/* { dg-final { scan-tree-dump-times "BIT_FIELD_REF" 2 "reassoc1" { target { powerpc*-*-* i?86-*-* x86_64-*-* } } } } */
