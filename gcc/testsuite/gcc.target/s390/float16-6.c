/* { dg-do run { target float16 } } */
/* { dg-options "-O2 -save-temps" } */
/* { dg-final { scan-assembler-times "brasl\t%r14,__truncsfhf2@PLT" 1 } } */
/* { dg-final { scan-assembler-times "brasl\t%r14,__truncdfhf2@PLT" 1 } } */
/* { dg-final { scan-assembler-times "brasl\t%r14,__trunctfhf2@PLT" 1 } } */

#include <assert.h>
#include <fenv.h>

#define fn_truncate(mode, type) \
  [[gnu::noipa]] _Float16 \
  truncate##mode##hf (type x) { return x; }

fn_truncate (sf, float)
fn_truncate (df, double)
fn_truncate (tf, long double)

#define fn_test_truncate(mode, type) \
void \
test_truncate##mode (type x, unsigned short a, unsigned short b, \
                     unsigned short c, unsigned short d) \
{ \
  _Float16 y; \
  unsigned short z; \
\
  fesetround (FE_TONEAREST); \
  y = truncate##mode##hf (x); \
  __builtin_memcpy (&z, &y, sizeof (z)); \
  assert (z == a); \
\
  fesetround (FE_TOWARDZERO); \
  y = truncate##mode##hf (x); \
  __builtin_memcpy (&z, &y, sizeof (z)); \
  assert (z == b); \
\
  fesetround (FE_DOWNWARD); \
  y = truncate##mode##hf (x); \
  __builtin_memcpy (&z, &y, sizeof (z)); \
  assert (z == c); \
\
  fesetround (FE_UPWARD); \
  y = truncate##mode##hf (x); \
  __builtin_memcpy (&z, &y, sizeof (z)); \
  assert (z == d); \
}

fn_test_truncate (sf, float)
fn_test_truncate (df, double)
fn_test_truncate (tf, long double)

int
main (void)
{
  test_truncatesf (__FLT_MAX__, 0x7c00, 0x7bff, 0x7bff, 0x7c00);
  test_truncatesf (__FLT_MIN__, 0, 0, 0, 1);
  test_truncatesf (0.5f, 0x3800, 0x3800, 0x3800, 0x3800);
  test_truncatesf (-0.0000001f, 0x8002, 0x8001, 0x8002, 0x8001);
  test_truncatesf (0.0000001f, 2, 1, 1, 2);

  test_truncatedf (__DBL_MAX__, 0x7c00, 0x7bff, 0x7bff, 0x7c00);
  test_truncatedf (__DBL_MIN__, 0, 0, 0, 1);
  test_truncatedf (0.5f, 0x3800, 0x3800, 0x3800, 0x3800);
  test_truncatedf (-0.0000001f, 0x8002, 0x8001, 0x8002, 0x8001);
  test_truncatedf (0.0000001f, 2, 1, 1, 2);

  test_truncatetf (__LDBL_MAX__, 0x7c00, 0x7bff, 0x7bff, 0x7c00);
  test_truncatetf (__LDBL_MIN__, 0, 0, 0, 1);
  test_truncatetf (0.5f, 0x3800, 0x3800, 0x3800, 0x3800);
  test_truncatetf (-0.0000001f, 0x8002, 0x8001, 0x8002, 0x8001);
  test_truncatetf (0.0000001f, 2, 1, 1, 2);
}
