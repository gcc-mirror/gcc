/* { dg-do compile }  */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O3" } */
/* { dg-add-options arm_neon } */

typedef __simd64_int32_t int32x2_t;
typedef __attribute__((aligned (1))) int32x2_t unalignedvec;

unalignedvec a = {11, 13};

void foo(unalignedvec *);

void test()
{
  unalignedvec x = a;
  foo (&x);
  a = x;
}

/* { dg-final { scan-assembler-times "vld1.32" 1 } } */
/* { dg-final { scan-assembler-times "vst1.32" 1 } } */
/* { dg-final { scan-assembler-times "vldr" 1 } } */
/* { dg-final { scan-assembler-times "vstr" 1 } } */
