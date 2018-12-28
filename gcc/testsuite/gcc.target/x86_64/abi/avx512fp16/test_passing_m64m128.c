#include <stdio.h>
#include "avx512fp16-xmm-check.h"
#include "defines.h"
#include "macros.h"
#include "args.h"

struct IntegerRegisters iregs;
struct FloatRegisters fregs;
unsigned int num_iregs, num_fregs;

/* This struct holds values for argument checking.  */
struct
{
  XMM_T i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15,
    i16, i17, i18, i19, i20, i21, i22, i23;
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
fun_check_passing_m64_8_values (__m64 i0 ATTRIBUTE_UNUSED,
				__m64 i1 ATTRIBUTE_UNUSED,
				__m64 i2 ATTRIBUTE_UNUSED,
				__m64 i3 ATTRIBUTE_UNUSED,
				__m64 i4 ATTRIBUTE_UNUSED,
				__m64 i5 ATTRIBUTE_UNUSED,
				__m64 i6 ATTRIBUTE_UNUSED,
				__m64 i7 ATTRIBUTE_UNUSED)
{
  /* Check argument values.  */
  compare (values.i0, i0, __m64);
  compare (values.i1, i1, __m64);
  compare (values.i2, i2, __m64);
  compare (values.i3, i3, __m64);
  compare (values.i4, i4, __m64);
  compare (values.i5, i5, __m64);
  compare (values.i6, i6, __m64);
  compare (values.i7, i7, __m64);
}

void
fun_check_passing_m64_8_regs (__m64 i0 ATTRIBUTE_UNUSED,
			      __m64 i1 ATTRIBUTE_UNUSED,
			      __m64 i2 ATTRIBUTE_UNUSED,
			      __m64 i3 ATTRIBUTE_UNUSED,
			      __m64 i4 ATTRIBUTE_UNUSED,
			      __m64 i5 ATTRIBUTE_UNUSED,
			      __m64 i6 ATTRIBUTE_UNUSED,
			      __m64 i7 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_m64_arguments;
}

void
fun_check_passing_m64_20_values (__m64 i0 ATTRIBUTE_UNUSED,
				 __m64 i1 ATTRIBUTE_UNUSED,
				 __m64 i2 ATTRIBUTE_UNUSED,
				 __m64 i3 ATTRIBUTE_UNUSED,
				 __m64 i4 ATTRIBUTE_UNUSED,
				 __m64 i5 ATTRIBUTE_UNUSED,
				 __m64 i6 ATTRIBUTE_UNUSED,
				 __m64 i7 ATTRIBUTE_UNUSED,
				 __m64 i8 ATTRIBUTE_UNUSED,
				 __m64 i9 ATTRIBUTE_UNUSED,
				 __m64 i10 ATTRIBUTE_UNUSED,
				 __m64 i11 ATTRIBUTE_UNUSED,
				 __m64 i12 ATTRIBUTE_UNUSED,
				 __m64 i13 ATTRIBUTE_UNUSED,
				 __m64 i14 ATTRIBUTE_UNUSED,
				 __m64 i15 ATTRIBUTE_UNUSED,
				 __m64 i16 ATTRIBUTE_UNUSED,
				 __m64 i17 ATTRIBUTE_UNUSED,
				 __m64 i18 ATTRIBUTE_UNUSED,
				 __m64 i19 ATTRIBUTE_UNUSED)
{
  /* Check argument values.  */
  compare (values.i0, i0, __m64);
  compare (values.i1, i1, __m64);
  compare (values.i2, i2, __m64);
  compare (values.i3, i3, __m64);
  compare (values.i4, i4, __m64);
  compare (values.i5, i5, __m64);
  compare (values.i6, i6, __m64);
  compare (values.i7, i7, __m64);
  compare (values.i8, i8, __m64);
  compare (values.i9, i9, __m64);
  compare (values.i10, i10, __m64);
  compare (values.i11, i11, __m64);
  compare (values.i12, i12, __m64);
  compare (values.i13, i13, __m64);
  compare (values.i14, i14, __m64);
  compare (values.i15, i15, __m64);
  compare (values.i16, i16, __m64);
  compare (values.i17, i17, __m64);
  compare (values.i18, i18, __m64);
  compare (values.i19, i19, __m64);
}

void
fun_check_passing_m64_20_regs (__m64 i0 ATTRIBUTE_UNUSED,
			       __m64 i1 ATTRIBUTE_UNUSED,
			       __m64 i2 ATTRIBUTE_UNUSED,
			       __m64 i3 ATTRIBUTE_UNUSED,
			       __m64 i4 ATTRIBUTE_UNUSED,
			       __m64 i5 ATTRIBUTE_UNUSED,
			       __m64 i6 ATTRIBUTE_UNUSED,
			       __m64 i7 ATTRIBUTE_UNUSED,
			       __m64 i8 ATTRIBUTE_UNUSED,
			       __m64 i9 ATTRIBUTE_UNUSED,
			       __m64 i10 ATTRIBUTE_UNUSED,
			       __m64 i11 ATTRIBUTE_UNUSED,
			       __m64 i12 ATTRIBUTE_UNUSED,
			       __m64 i13 ATTRIBUTE_UNUSED,
			       __m64 i14 ATTRIBUTE_UNUSED,
			       __m64 i15 ATTRIBUTE_UNUSED,
			       __m64 i16 ATTRIBUTE_UNUSED,
			       __m64 i17 ATTRIBUTE_UNUSED,
			       __m64 i18 ATTRIBUTE_UNUSED,
			       __m64 i19 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_m64_arguments;
}

void
fun_check_passing_m128_8_values (__m128 i0 ATTRIBUTE_UNUSED,
				 __m128 i1 ATTRIBUTE_UNUSED,
				 __m128 i2 ATTRIBUTE_UNUSED,
				 __m128 i3 ATTRIBUTE_UNUSED,
				 __m128 i4 ATTRIBUTE_UNUSED,
				 __m128 i5 ATTRIBUTE_UNUSED,
				 __m128 i6 ATTRIBUTE_UNUSED,
				 __m128 i7 ATTRIBUTE_UNUSED)
{
  /* Check argument values.  */
  compare (values.i0, i0, __m128);
  compare (values.i1, i1, __m128);
  compare (values.i2, i2, __m128);
  compare (values.i3, i3, __m128);
  compare (values.i4, i4, __m128);
  compare (values.i5, i5, __m128);
  compare (values.i6, i6, __m128);
  compare (values.i7, i7, __m128);
}

void
fun_check_passing_m128h_8_values (__m128h i0 ATTRIBUTE_UNUSED,
				  __m128h i1 ATTRIBUTE_UNUSED,
				  __m128h i2 ATTRIBUTE_UNUSED,
				  __m128h i3 ATTRIBUTE_UNUSED,
				  __m128h i4 ATTRIBUTE_UNUSED,
				  __m128h i5 ATTRIBUTE_UNUSED,
				  __m128h i6 ATTRIBUTE_UNUSED,
				  __m128h i7 ATTRIBUTE_UNUSED)
{
  /* Check argument values.  */
  compare (values.i0, i0, __m128h);
  compare (values.i1, i1, __m128h);
  compare (values.i2, i2, __m128h);
  compare (values.i3, i3, __m128h);
  compare (values.i4, i4, __m128h);
  compare (values.i5, i5, __m128h);
  compare (values.i6, i6, __m128h);
  compare (values.i7, i7, __m128h);
}

void
fun_check_passing_m128_8_regs (__m128 i0 ATTRIBUTE_UNUSED,
			       __m128 i1 ATTRIBUTE_UNUSED,
			       __m128 i2 ATTRIBUTE_UNUSED,
			       __m128 i3 ATTRIBUTE_UNUSED,
			       __m128 i4 ATTRIBUTE_UNUSED,
			       __m128 i5 ATTRIBUTE_UNUSED,
			       __m128 i6 ATTRIBUTE_UNUSED,
			       __m128 i7 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_m128_arguments;
}

void
fun_check_passing_m128h_8_regs (__m128h i0 ATTRIBUTE_UNUSED,
			        __m128h i1 ATTRIBUTE_UNUSED,
			        __m128h i2 ATTRIBUTE_UNUSED,
			        __m128h i3 ATTRIBUTE_UNUSED,
			        __m128h i4 ATTRIBUTE_UNUSED,
			        __m128h i5 ATTRIBUTE_UNUSED,
			        __m128h i6 ATTRIBUTE_UNUSED,
			        __m128h i7 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_m128_arguments;
}

void
fun_check_passing_m128_20_values (__m128 i0 ATTRIBUTE_UNUSED,
				  __m128 i1 ATTRIBUTE_UNUSED,
				  __m128 i2 ATTRIBUTE_UNUSED,
				  __m128 i3 ATTRIBUTE_UNUSED,
				  __m128 i4 ATTRIBUTE_UNUSED,
				  __m128 i5 ATTRIBUTE_UNUSED,
				  __m128 i6 ATTRIBUTE_UNUSED,
				  __m128 i7 ATTRIBUTE_UNUSED,
				  __m128 i8 ATTRIBUTE_UNUSED,
				  __m128 i9 ATTRIBUTE_UNUSED,
				  __m128 i10 ATTRIBUTE_UNUSED,
				  __m128 i11 ATTRIBUTE_UNUSED,
				  __m128 i12 ATTRIBUTE_UNUSED,
				  __m128 i13 ATTRIBUTE_UNUSED,
				  __m128 i14 ATTRIBUTE_UNUSED,
				  __m128 i15 ATTRIBUTE_UNUSED,
				  __m128 i16 ATTRIBUTE_UNUSED,
				  __m128 i17 ATTRIBUTE_UNUSED,
				  __m128 i18 ATTRIBUTE_UNUSED,
				  __m128 i19 ATTRIBUTE_UNUSED)
{
  /* Check argument values.  */
  compare (values.i0, i0, __m128);
  compare (values.i1, i1, __m128);
  compare (values.i2, i2, __m128);
  compare (values.i3, i3, __m128);
  compare (values.i4, i4, __m128);
  compare (values.i5, i5, __m128);
  compare (values.i6, i6, __m128);
  compare (values.i7, i7, __m128);
  compare (values.i8, i8, __m128);
  compare (values.i9, i9, __m128);
  compare (values.i10, i10, __m128);
  compare (values.i11, i11, __m128);
  compare (values.i12, i12, __m128);
  compare (values.i13, i13, __m128);
  compare (values.i14, i14, __m128);
  compare (values.i15, i15, __m128);
  compare (values.i16, i16, __m128);
  compare (values.i17, i17, __m128);
  compare (values.i18, i18, __m128);
  compare (values.i19, i19, __m128);
}

void
fun_check_passing_m128h_20_values (__m128h i0 ATTRIBUTE_UNUSED,
				   __m128h i1 ATTRIBUTE_UNUSED,
				   __m128h i2 ATTRIBUTE_UNUSED,
				   __m128h i3 ATTRIBUTE_UNUSED,
				   __m128h i4 ATTRIBUTE_UNUSED,
				   __m128h i5 ATTRIBUTE_UNUSED,
				   __m128h i6 ATTRIBUTE_UNUSED,
				   __m128h i7 ATTRIBUTE_UNUSED,
				   __m128h i8 ATTRIBUTE_UNUSED,
				   __m128h i9 ATTRIBUTE_UNUSED,
				   __m128h i10 ATTRIBUTE_UNUSED,
				   __m128h i11 ATTRIBUTE_UNUSED,
				   __m128h i12 ATTRIBUTE_UNUSED,
				   __m128h i13 ATTRIBUTE_UNUSED,
				   __m128h i14 ATTRIBUTE_UNUSED,
				   __m128h i15 ATTRIBUTE_UNUSED,
				   __m128h i16 ATTRIBUTE_UNUSED,
				   __m128h i17 ATTRIBUTE_UNUSED,
				   __m128h i18 ATTRIBUTE_UNUSED,
				   __m128h i19 ATTRIBUTE_UNUSED)
{
  /* Check argument values.  */
  compare (values.i0, i0, __m128h);
  compare (values.i1, i1, __m128h);
  compare (values.i2, i2, __m128h);
  compare (values.i3, i3, __m128h);
  compare (values.i4, i4, __m128h);
  compare (values.i5, i5, __m128h);
  compare (values.i6, i6, __m128h);
  compare (values.i7, i7, __m128h);
  compare (values.i8, i8, __m128h);
  compare (values.i9, i9, __m128h);
  compare (values.i10, i10, __m128h);
  compare (values.i11, i11, __m128h);
  compare (values.i12, i12, __m128h);
  compare (values.i13, i13, __m128h);
  compare (values.i14, i14, __m128h);
  compare (values.i15, i15, __m128h);
  compare (values.i16, i16, __m128h);
  compare (values.i17, i17, __m128h);
  compare (values.i18, i18, __m128h);
  compare (values.i19, i19, __m128h);
}

void
fun_check_passing_m128_20_regs (__m128 i0 ATTRIBUTE_UNUSED,
				__m128 i1 ATTRIBUTE_UNUSED,
				__m128 i2 ATTRIBUTE_UNUSED,
				__m128 i3 ATTRIBUTE_UNUSED,
				__m128 i4 ATTRIBUTE_UNUSED,
				__m128 i5 ATTRIBUTE_UNUSED,
				__m128 i6 ATTRIBUTE_UNUSED,
				__m128 i7 ATTRIBUTE_UNUSED,
				__m128 i8 ATTRIBUTE_UNUSED,
				__m128 i9 ATTRIBUTE_UNUSED,
				__m128 i10 ATTRIBUTE_UNUSED,
				__m128 i11 ATTRIBUTE_UNUSED,
				__m128 i12 ATTRIBUTE_UNUSED,
				__m128 i13 ATTRIBUTE_UNUSED,
				__m128 i14 ATTRIBUTE_UNUSED,
				__m128 i15 ATTRIBUTE_UNUSED,
				__m128 i16 ATTRIBUTE_UNUSED,
				__m128 i17 ATTRIBUTE_UNUSED,
				__m128 i18 ATTRIBUTE_UNUSED,
				__m128 i19 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_m128_arguments;
}

void
fun_check_passing_m128h_20_regs (__m128h i0 ATTRIBUTE_UNUSED,
				 __m128h i1 ATTRIBUTE_UNUSED,
				 __m128h i2 ATTRIBUTE_UNUSED,
				 __m128h i3 ATTRIBUTE_UNUSED,
				 __m128h i4 ATTRIBUTE_UNUSED,
				 __m128h i5 ATTRIBUTE_UNUSED,
				 __m128h i6 ATTRIBUTE_UNUSED,
				 __m128h i7 ATTRIBUTE_UNUSED,
				 __m128h i8 ATTRIBUTE_UNUSED,
				 __m128h i9 ATTRIBUTE_UNUSED,
				 __m128h i10 ATTRIBUTE_UNUSED,
				 __m128h i11 ATTRIBUTE_UNUSED,
				 __m128h i12 ATTRIBUTE_UNUSED,
				 __m128h i13 ATTRIBUTE_UNUSED,
				 __m128h i14 ATTRIBUTE_UNUSED,
				 __m128h i15 ATTRIBUTE_UNUSED,
				 __m128h i16 ATTRIBUTE_UNUSED,
				 __m128h i17 ATTRIBUTE_UNUSED,
				 __m128h i18 ATTRIBUTE_UNUSED,
				 __m128h i19 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_m128_arguments;
}

#define def_check_int_passing8(_i0, _i1, _i2, _i3, \
			       _i4, _i5, _i6, _i7, \
			       _func1, _func2, TYPE) \
  values.i0.TYPE[0] = _i0; \
  values.i1.TYPE[0] = _i1; \
  values.i2.TYPE[0] = _i2; \
  values.i3.TYPE[0] = _i3; \
  values.i4.TYPE[0] = _i4; \
  values.i5.TYPE[0] = _i5; \
  values.i6.TYPE[0] = _i6; \
  values.i7.TYPE[0] = _i7; \
  WRAP_CALL(_func1) (_i0, _i1, _i2, _i3, _i4, _i5, _i6, _i7); \
  clear_float_registers; \
  fregs.F0.TYPE[0] = _i0; \
  fregs.F1.TYPE[0] = _i1; \
  fregs.F2.TYPE[0] = _i2; \
  fregs.F3.TYPE[0] = _i3; \
  fregs.F4.TYPE[0] = _i4; \
  fregs.F5.TYPE[0] = _i5; \
  fregs.F6.TYPE[0] = _i6; \
  fregs.F7.TYPE[0] = _i7; \
  num_fregs = 8; \
  WRAP_CALL(_func2) (_i0, _i1, _i2, _i3, _i4, _i5, _i6, _i7);

#define def_check_int_passing20(_i0, _i1, _i2, _i3, _i4, _i5, _i6, \
				_i7, _i8, _i9, _i10, _i11, _i12, _i13, \
				_i14, _i15, _i16, _i17, _i18, _i19, \
				_func1, _func2, TYPE) \
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
  values.i10.TYPE[0] = _i10; \
  values.i11.TYPE[0] = _i11; \
  values.i12.TYPE[0] = _i12; \
  values.i13.TYPE[0] = _i13; \
  values.i14.TYPE[0] = _i14; \
  values.i15.TYPE[0] = _i15; \
  values.i16.TYPE[0] = _i16; \
  values.i17.TYPE[0] = _i17; \
  values.i18.TYPE[0] = _i18; \
  values.i19.TYPE[0] = _i19; \
  WRAP_CALL(_func1) (_i0, _i1, _i2, _i3, _i4, _i5, _i6, _i7, _i8, \
		     _i9, _i10, _i11, _i12, _i13, _i14, _i15, _i16, \
		     _i17, _i18, _i19); \
  clear_float_registers; \
  fregs.F0.TYPE[0] = _i0; \
  fregs.F1.TYPE[0] = _i1; \
  fregs.F2.TYPE[0] = _i2; \
  fregs.F3.TYPE[0] = _i3; \
  fregs.F4.TYPE[0] = _i4; \
  fregs.F5.TYPE[0] = _i5; \
  fregs.F6.TYPE[0] = _i6; \
  fregs.F7.TYPE[0] = _i7; \
  num_fregs = 8; \
  WRAP_CALL(_func2) (_i0, _i1, _i2, _i3, _i4, _i5, _i6, _i7, _i8, \
		     _i9, _i10, _i11, _i12, _i13, _i14, _i15, _i16, \
		     _i17, _i18, _i19);

void
test_m64_on_stack ()
{
  __m64 x[8];
  int i;
  for (i = 0; i < 8; i++)
    x[i] = (__m64){32 + i, 0};
  pass = "m64-8";
  def_check_int_passing8 (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7],
			  fun_check_passing_m64_8_values,
			  fun_check_passing_m64_8_regs, _m64);
}

void
test_too_many_m64 ()
{
  __m64 x[20];
  int i;
  for (i = 0; i < 20; i++)
    x[i] = (__m64){32 + i, 0};
  pass = "m64-20";
  def_check_int_passing20 (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7],
			   x[8], x[9], x[10], x[11], x[12], x[13], x[14],
			   x[15], x[16], x[17], x[18], x[19],
			   fun_check_passing_m64_20_values,
			   fun_check_passing_m64_20_regs, _m64);
}

void
test_m128_on_stack ()
{
  __m128 x[8];
  int i;
  for (i = 0; i < 8; i++)
    x[i] = (__m128){32 + i, 0, 0, 0};
  pass = "m128-8";
  def_check_int_passing8 (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7],
			  fun_check_passing_m128_8_values,
			  fun_check_passing_m128_8_regs, _m128);
}

void
test_m128h_on_stack ()
{
  __m128h x[8];
  int i;
  for (i = 0; i < 8; i++)
    x[i] = (__m128h){1.1f16, 2.2f16, 3.3f16, 4.4f16, 5.5f16,
	             6.6f16, 7.7f16, 8.8f16};
  pass = "m128h-8";
  def_check_int_passing8 (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7],
			  fun_check_passing_m128h_8_values,
			  fun_check_passing_m128h_8_regs, _m128h);
}

void
test_too_many_m128 ()
{
  __m128 x[20];
  int i;
  for (i = 0; i < 20; i++)
    x[i] = (__m128){32 + i, 0, 0, 0};
  pass = "m128-20";
  def_check_int_passing20 (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7],
			   x[8], x[9], x[10], x[11], x[12], x[13], x[14],
			   x[15], x[16], x[17], x[18], x[19],
			   fun_check_passing_m128_20_values,
			   fun_check_passing_m128_20_regs, _m128);
}

void
test_too_many_m128h ()
{
  __m128h x[20];
  int i;
  for (i = 0; i < 20; i++)
    x[i] = (__m128h){1.1f16, 2.2f16, 3.3f16, 4.4f16, 5.5f16,
	             6.6f16, 7.7f16, 8.8f16};
  pass = "m128h-20";
  def_check_int_passing20 (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7],
			   x[8], x[9], x[10], x[11], x[12], x[13], x[14],
			   x[15], x[16], x[17], x[18], x[19],
			   fun_check_passing_m128h_20_values,
			   fun_check_passing_m128h_20_regs, _m128h);
}

static void
do_test (void)
{
  test_m64_on_stack ();
  test_too_many_m64 ();
  test_m128_on_stack ();
  test_too_many_m128 ();
  test_m128h_on_stack ();
  test_too_many_m128h ();
  if (failed)
    abort ();
}
