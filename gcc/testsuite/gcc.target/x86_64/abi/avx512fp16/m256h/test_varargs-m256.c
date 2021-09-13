/* Test variable number of 256-bit vector arguments passed to functions.  */

#include <stdio.h>
#include "avx512fp16-ymm-check.h"
#include "args.h"

struct IntegerRegisters iregs;
struct FloatRegisters fregs;

/* This struct holds values for argument checking.  */
struct 
{
  YMM_T i0, i1, i2, i3, i4, i5, i6, i7, i8, i9;
} values;

char *pass;
int failed = 0;

#undef assert
#define assert(c) do { \
  if (!(c)) {failed++; printf ("failed %s\n", pass); } \
} while (0)

#define compare(X1,X2,T) do { \
  assert (memcmp (&X1, &X2, sizeof (T)) == 0); \
} while (0)

void
fun_check_passing_m256_varargs (__m256 i0, __m256 i1, __m256 i2,
				__m256 i3, ...)
{
  /* Check argument values.  */
  void **fp = __builtin_frame_address (0);
  void *ra = __builtin_return_address (0);
  __m256 *argp;

  compare (values.i0, i0, __m256);
  compare (values.i1, i1, __m256);
  compare (values.i2, i2, __m256);
  compare (values.i3, i3, __m256);

  /* Get the pointer to the return address on stack.  */
  while (*fp != ra)
    fp++;

  /* Skip the return address stack slot.  */
  argp = (__m256 *)(((char *) fp) + 8);

  /* Check __m256 arguments passed on stack.  */
  compare (values.i4, argp[0], __m256);
  compare (values.i5, argp[1], __m256);
  compare (values.i6, argp[2], __m256);
  compare (values.i7, argp[3], __m256);
  compare (values.i8, argp[4], __m256);
  compare (values.i9, argp[5], __m256);

  /* Check register contents.  */
  compare (fregs.ymm0, ymm_regs[0], __m256);
  compare (fregs.ymm1, ymm_regs[1], __m256);
  compare (fregs.ymm2, ymm_regs[2], __m256);
  compare (fregs.ymm3, ymm_regs[3], __m256);
}

void
fun_check_passing_m256h_varargs (__m256h i0, __m256h i1, __m256h i2,
				 __m256h i3, ...)
{
  /* Check argument values.  */
  void **fp = __builtin_frame_address (0);
  void *ra = __builtin_return_address (0);
  __m256h *argp;

  compare (values.i0, i0, __m256h);
  compare (values.i1, i1, __m256h);
  compare (values.i2, i2, __m256h);
  compare (values.i3, i3, __m256h);

  /* Get the pointer to the return address on stack.  */
  while (*fp != ra)
    fp++;

  /* Skip the return address stack slot.  */
  argp = (__m256h *)(((char *) fp) + 8);

  /* Check __m256h arguments passed on stack.  */
  compare (values.i4, argp[0], __m256h);
  compare (values.i5, argp[1], __m256h);
  compare (values.i6, argp[2], __m256h);
  compare (values.i7, argp[3], __m256h);
  compare (values.i8, argp[4], __m256h);
  compare (values.i9, argp[5], __m256h);

  /* Check register contents.  */
  compare (fregs.ymm0, ymm_regs[0], __m256h);
  compare (fregs.ymm1, ymm_regs[1], __m256h);
  compare (fregs.ymm2, ymm_regs[2], __m256h);
  compare (fregs.ymm3, ymm_regs[3], __m256h);
}

#define def_check_int_passing_varargs(_i0, _i1, _i2, _i3, _i4, _i5, \
				      _i6, _i7, _i8, _i9, \
				      _func, TYPE) \
  values.i0.TYPE[0] = _i0; \
  values.i1.TYPE[0] = _i1; \
  values.i2.TYPE[0] = _i2; \
  values.i3.TYPE[0] = _i3; \
  values.i4.TYPE[0] = _i4; \
  values.i5.TYPE[0] = _i5; \
  values.i6.TYPE[0] = _i6; \
  values.i7.TYPE[0] = _i7; \
  values.i8.TYPE[0] = _i8; \
  values.i9.TYPE[0] = _i9; \
  clear_struct_registers; \
  fregs.F0.TYPE[0] = _i0; \
  fregs.F1.TYPE[0] = _i1; \
  fregs.F2.TYPE[0] = _i2; \
  fregs.F3.TYPE[0] = _i3; \
  WRAP_CALL(_func) (_i0, _i1, _i2, _i3, _i4, _i5, _i6, _i7, _i8, _i9);

void
test_m256_varargs (void)
{
  __m256 x[10];
  int i;
  for (i = 0; i < 10; i++)
    x[i] = (__m256){32+i, 0, 0, 0, 0, 0, 0, 0};
  pass = "m256-varargs";
  def_check_int_passing_varargs (x[0], x[1], x[2], x[3], x[4], x[5],
				 x[6], x[7], x[8], x[9],
				 fun_check_passing_m256_varargs,
				 _m256);
}

void
test_m256h_varargs (void)
{
  __m256h x[10];
  int i;
  for (i = 0; i < 10; i++)
    x[i] = (__m256h) {
        1.1f16 + i, 2.2f16 + i, 3.3f16 + i, 4.4f16 + i,
	5.5f16 + i, 6.6f16 + i, 7.7f16 + i, 8.8f16 + i,
	9.9f16 + i, 10.10f16 + i, 11.11f16 + i, 12.12f16 + i,
	13.13f16 + i, 14.14f16 + i, 15.15f16 + i, 16.16f16 + i
    };
  pass = "m256h-varargs";
  def_check_int_passing_varargs (x[0], x[1], x[2], x[3], x[4], x[5],
				 x[6], x[7], x[8], x[9],
				 fun_check_passing_m256h_varargs,
				 _m256h);
}

void
do_test (void)
{
  test_m256_varargs ();
  test_m256h_varargs ();
  if (failed)
    abort ();
}
