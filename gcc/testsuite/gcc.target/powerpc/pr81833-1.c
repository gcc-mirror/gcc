/* PR81833: This used to fail due to improper implementation of vec_msum.  */
/* Test case relies on -mcpu=power7 or later.  Currently we don't have
   machinery to express that, so we have two separate tests for -mcpu=power7
   and -mcpu=power8 to catch 32-bit BE on P7 and 64-bit BE/LE on P8.  */

/* { dg-do run } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O2" } */

#include <altivec.h>

#define vec_u8  vector unsigned char
#define vec_s8  vector signed char
#define vec_u16 vector unsigned short
#define vec_s16 vector signed short
#define vec_u32 vector unsigned int
#define vec_s32 vector signed int
#define vec_f   vector float

#define LOAD_ZERO const vec_u8 zerov = vec_splat_u8 (0)

#define zero_u8v  (vec_u8)  zerov
#define zero_s8v  (vec_s8)  zerov
#define zero_u16v (vec_u16) zerov
#define zero_s16v (vec_s16) zerov
#define zero_u32v (vec_u32) zerov
#define zero_s32v (vec_s32) zerov

signed int __attribute__((noinline))
scalarproduct_int16_vsx (const signed short *v1, const signed short *v2,
			 int order)
{
  int i;
  LOAD_ZERO;
  register vec_s16 vec1;
  register vec_s32 res = vec_splat_s32 (0), t;
  signed int ires;

  for (i = 0; i < order; i += 8) {
    vec1 = vec_vsx_ld (0, v1);
    t    = vec_msum (vec1, vec_vsx_ld (0, v2), zero_s32v);
    res  = vec_sums (t, res);
    v1  += 8;
    v2  += 8;
  }
  res = vec_splat (res, 3);
  vec_ste (res, 0, &ires);

  return ires;
}

int main(void)
{
  const signed short test_vec[] = { 1, 1, 1, 1, 1, 1, 1, 1 };
  if (scalarproduct_int16_vsx (test_vec, test_vec, 8) != 8)
    __builtin_abort ();
  return 0;
}
