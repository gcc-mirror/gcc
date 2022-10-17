/* { dg-do compile } */
/* { dg-options "-O0 -mavx256-split-unaligned-store -mavx -fpack-struct" } */

typedef __bf16 __m256bf16 __attribute__((__vector_size__(32)));
typedef struct {
  __m256bf16 _m256bf16[1];
} YMM_T;

struct {
  YMM_T ymm0;
} fregs;

__m256bf16 do_test_u3b_0_0;
int do_test_i;

void
do_test()
{
  (&fregs.ymm0)[do_test_i]._m256bf16[0] = do_test_u3b_0_0;
}
