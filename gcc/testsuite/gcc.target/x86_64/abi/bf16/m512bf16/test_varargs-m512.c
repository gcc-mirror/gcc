/* Test variable number of 512-bit vector arguments passed to functions.  */

#include <stdio.h>
#include "bf16-zmm-check.h"
#include "args.h"

struct FloatRegisters fregs;
struct IntegerRegisters iregs;

/* This struct holds values for argument checking.  */
struct 
{
  ZMM_T i0, i1, i2, i3, i4, i5, i6, i7, i8, i9;
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
fun_check_passing_m512bf16_varargs (__m512bf16 i0, __m512bf16 i1, __m512bf16 i2,
				 __m512bf16 i3, ...)
{
  /* Check argument values.  */
  void **fp = __builtin_frame_address (0);
  void *ra = __builtin_return_address (0);
  __m512bf16 *argp;

  compare (values.i0, i0, __m512bf16);
  compare (values.i1, i1, __m512bf16);
  compare (values.i2, i2, __m512bf16);
  compare (values.i3, i3, __m512bf16);

  /* Get the pointer to the return address on stack.  */
  while (*fp != ra)
    fp++;

  /* Skip the return address stack slot.  */
  argp = (__m512bf16 *)(((char *) fp) + 8);

  /* Check __m512bf16 arguments passed on stack.  */
  compare (values.i4, argp[0], __m512bf16);
  compare (values.i5, argp[1], __m512bf16);
  compare (values.i6, argp[2], __m512bf16);
  compare (values.i7, argp[3], __m512bf16);
  compare (values.i8, argp[4], __m512bf16);
  compare (values.i9, argp[5], __m512bf16);

  /* Check register contents.  */
  compare (fregs.zmm0, zmm_regs[0], __m512bf16);
  compare (fregs.zmm1, zmm_regs[1], __m512bf16);
  compare (fregs.zmm2, zmm_regs[2], __m512bf16);
  compare (fregs.zmm3, zmm_regs[3], __m512bf16);
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
test_m512bf16_varargs (void)
{
  __m512bf16 x[10];
  __bf16 bf1, bf2, bf3, bf4, bf5, bf6, bf7, bf8,
	 bf9, bf10,bf11,bf12,bf13,bf14,bf15,bf16,
	 bf17,bf18,bf19,bf20,bf21,bf22,bf23,bf24,
	 bf25,bf26,bf27,bf28,bf29,bf30,bf31,bf32;
  int i;
  for (i = 0; i < 10; i++)
    x[i] = (__m512bf16) { bf1, bf2, bf3, bf4, bf5, bf6, bf7, bf8,
			  bf9, bf10,bf11,bf12,bf13,bf14,bf15,bf16,
			  bf17,bf18,bf19,bf20,bf21,bf22,bf23,bf24,
			  bf25,bf26,bf27,bf28,bf29,bf30,bf31,bf32 };
  pass = "m512bf16-varargs";
  def_check_int_passing_varargs (x[0], x[1], x[2], x[3], x[4], x[5],
				 x[6], x[7], x[8], x[9],
				 fun_check_passing_m512bf16_varargs,
				 _m512bf16);
}

void
do_test (void)
{
  test_m512bf16_varargs ();
  if (failed)
    abort ();
}
