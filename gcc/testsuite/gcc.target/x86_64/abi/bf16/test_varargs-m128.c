/* Test variable number of 128-bit vector arguments passed to functions.  */

#include <stdio.h>
#include "bf16-check.h"
#include "defines.h"
#include "macros.h"
#include "args.h"

struct FloatRegisters fregs;
struct IntegerRegisters iregs;

/* This struct holds values for argument checking.  */
struct 
{
  XMM_T i0, i1, i2, i3, i4, i5, i6, i7, i8, i9;
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
fun_check_passing_m128bf16_varargs (__m128bf16 i0, __m128bf16 i1, __m128bf16 i2,
				 __m128bf16 i3, ...)
{
  /* Check argument values.  */
  void **fp = __builtin_frame_address (0);
  void *ra = __builtin_return_address (0);
  __m128bf16 *argp;

  compare (values.i0, i0, __m128bf16);
  compare (values.i1, i1, __m128bf16);
  compare (values.i2, i2, __m128bf16);
  compare (values.i3, i3, __m128bf16);

  /* Get the pointer to the return address on stack.  */
  while (*fp != ra)
    fp++;

  /* Skip the return address stack slot.  */
  argp = (__m128bf16 *) (((char *) fp) + 8);

  /* Check __m128bf16 arguments passed on stack.  */
  compare (values.i8, argp[0], __m128bf16);
  compare (values.i9, argp[1], __m128bf16);

  /* Check register contents.  */
  compare (fregs.xmm0, xmm_regs[0], __m128bf16);
  compare (fregs.xmm1, xmm_regs[1], __m128bf16);
  compare (fregs.xmm2, xmm_regs[2], __m128bf16);
  compare (fregs.xmm3, xmm_regs[3], __m128bf16);
  compare (fregs.xmm4, xmm_regs[4], __m128bf16);
  compare (fregs.xmm5, xmm_regs[5], __m128bf16);
  compare (fregs.xmm6, xmm_regs[6], __m128bf16);
  compare (fregs.xmm7, xmm_regs[7], __m128bf16);
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
  clear_float_registers; \
  fregs.F0.TYPE[0] = _i0; \
  fregs.F1.TYPE[0] = _i1; \
  fregs.F2.TYPE[0] = _i2; \
  fregs.F3.TYPE[0] = _i3; \
  fregs.F4.TYPE[0] = _i4; \
  fregs.F5.TYPE[0] = _i5; \
  fregs.F6.TYPE[0] = _i6; \
  fregs.F7.TYPE[0] = _i7; \
  WRAP_CALL(_func) (_i0, _i1, _i2, _i3, _i4, _i5, _i6, _i7, _i8, _i9);

void
test_m128bf16_varargs (void)
{
  __m128bf16 x[10];
  __bf16 bf1, bf2, bf3, bf4, bf5, bf6, bf7, bf8;
  int i;
  for (i = 0; i < 10; i++)
    x[i] = (__m128bf16) { bf1, bf2, bf3, bf4, bf5, bf6, bf7, bf8 };
  pass = "m128bf16-varargs";
  def_check_int_passing_varargs (x[0], x[1], x[2], x[3], x[4], x[5],
				 x[6], x[7], x[8], x[9],
				 fun_check_passing_m128bf16_varargs,
				 _m128bf16);
}

static void
do_test (void)
{
  test_m128bf16_varargs ();
  if (failed)
    abort ();
}
