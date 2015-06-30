#include "defines.h"
#include "macros.h"
#include "args.h"

struct IntegerRegisters iregbits = { ~0, ~0, ~0, ~0, ~0, ~0 };
struct IntegerRegisters iregs;
unsigned int num_iregs;

/* This struct holds values for argument checking.  */
struct
{
  int i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23;
} values_int;

struct
{
  long i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23;
} values_long;

void
fun_check_int_passing_int6_values (int i0 ATTRIBUTE_UNUSED, int i1 ATTRIBUTE_UNUSED, int i2 ATTRIBUTE_UNUSED, int i3 ATTRIBUTE_UNUSED, int i4 ATTRIBUTE_UNUSED, int i5 ATTRIBUTE_UNUSED)
{
  /* Check argument values.  */
  assert (values_int.i0 == i0);
  assert (values_int.i1 == i1);
  assert (values_int.i2 == i2);
  assert (values_int.i3 == i3);
  assert (values_int.i4 == i4);
  assert (values_int.i5 == i5);

}

void
fun_check_int_passing_int6_regs (int i0 ATTRIBUTE_UNUSED, int i1 ATTRIBUTE_UNUSED, int i2 ATTRIBUTE_UNUSED, int i3 ATTRIBUTE_UNUSED, int i4 ATTRIBUTE_UNUSED, int i5 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_int_arguments;
}

void
fun_check_int_passing_int12_values (int i0 ATTRIBUTE_UNUSED, int i1 ATTRIBUTE_UNUSED, int i2 ATTRIBUTE_UNUSED, int i3 ATTRIBUTE_UNUSED, int i4 ATTRIBUTE_UNUSED, int i5 ATTRIBUTE_UNUSED, int i6 ATTRIBUTE_UNUSED, int i7 ATTRIBUTE_UNUSED, int i8 ATTRIBUTE_UNUSED, int i9 ATTRIBUTE_UNUSED, int i10 ATTRIBUTE_UNUSED, int i11 ATTRIBUTE_UNUSED)
{
  /* Check argument values.  */
  assert (values_int.i0 == i0);
  assert (values_int.i1 == i1);
  assert (values_int.i2 == i2);
  assert (values_int.i3 == i3);
  assert (values_int.i4 == i4);
  assert (values_int.i5 == i5);
  assert (values_int.i6 == i6);
  assert (values_int.i7 == i7);
  assert (values_int.i8 == i8);
  assert (values_int.i9 == i9);
  assert (values_int.i10 == i10);
  assert (values_int.i11 == i11);

}

void
fun_check_int_passing_int12_regs (int i0 ATTRIBUTE_UNUSED, int i1 ATTRIBUTE_UNUSED, int i2 ATTRIBUTE_UNUSED, int i3 ATTRIBUTE_UNUSED, int i4 ATTRIBUTE_UNUSED, int i5 ATTRIBUTE_UNUSED, int i6 ATTRIBUTE_UNUSED, int i7 ATTRIBUTE_UNUSED, int i8 ATTRIBUTE_UNUSED, int i9 ATTRIBUTE_UNUSED, int i10 ATTRIBUTE_UNUSED, int i11 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_int_arguments;
}

void
fun_check_int_passing_long6_values (long i0 ATTRIBUTE_UNUSED, long i1 ATTRIBUTE_UNUSED, long i2 ATTRIBUTE_UNUSED, long i3 ATTRIBUTE_UNUSED, long i4 ATTRIBUTE_UNUSED, long i5 ATTRIBUTE_UNUSED)
{
  /* Check argument values.  */
  assert (values_long.i0 == i0);
  assert (values_long.i1 == i1);
  assert (values_long.i2 == i2);
  assert (values_long.i3 == i3);
  assert (values_long.i4 == i4);
  assert (values_long.i5 == i5);

}

void
fun_check_int_passing_long6_regs (long i0 ATTRIBUTE_UNUSED, long i1 ATTRIBUTE_UNUSED, long i2 ATTRIBUTE_UNUSED, long i3 ATTRIBUTE_UNUSED, long i4 ATTRIBUTE_UNUSED, long i5 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_long_arguments;
}

void
fun_check_int_passing_long12_values (long i0 ATTRIBUTE_UNUSED, long i1 ATTRIBUTE_UNUSED, long i2 ATTRIBUTE_UNUSED, long i3 ATTRIBUTE_UNUSED, long i4 ATTRIBUTE_UNUSED, long i5 ATTRIBUTE_UNUSED, long i6 ATTRIBUTE_UNUSED, long i7 ATTRIBUTE_UNUSED, long i8 ATTRIBUTE_UNUSED, long i9 ATTRIBUTE_UNUSED, long i10 ATTRIBUTE_UNUSED, long i11 ATTRIBUTE_UNUSED)
{
  /* Check argument values.  */
  assert (values_long.i0 == i0);
  assert (values_long.i1 == i1);
  assert (values_long.i2 == i2);
  assert (values_long.i3 == i3);
  assert (values_long.i4 == i4);
  assert (values_long.i5 == i5);
  assert (values_long.i6 == i6);
  assert (values_long.i7 == i7);
  assert (values_long.i8 == i8);
  assert (values_long.i9 == i9);
  assert (values_long.i10 == i10);
  assert (values_long.i11 == i11);

}

void
fun_check_int_passing_long12_regs (long i0 ATTRIBUTE_UNUSED, long i1 ATTRIBUTE_UNUSED, long i2 ATTRIBUTE_UNUSED, long i3 ATTRIBUTE_UNUSED, long i4 ATTRIBUTE_UNUSED, long i5 ATTRIBUTE_UNUSED, long i6 ATTRIBUTE_UNUSED, long i7 ATTRIBUTE_UNUSED, long i8 ATTRIBUTE_UNUSED, long i9 ATTRIBUTE_UNUSED, long i10 ATTRIBUTE_UNUSED, long i11 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_long_arguments;
}

#define def_check_int_passing6(_i0, _i1, _i2, _i3, _i4, _i5, _func1, _func2, TYPE) \
  values_ ## TYPE .i0 = _i0; \
  values_ ## TYPE .i1 = _i1; \
  values_ ## TYPE .i2 = _i2; \
  values_ ## TYPE .i3 = _i3; \
  values_ ## TYPE .i4 = _i4; \
  values_ ## TYPE .i5 = _i5; \
  WRAP_CALL(_func1) (_i0, _i1, _i2, _i3, _i4, _i5); \
  \
  clear_int_registers; \
  iregs.I0 = _i0; \
  iregs.I1 = _i1; \
  iregs.I2 = _i2; \
  num_iregs = 3; \
  WRAP_CALL(_func2) (_i0, _i1, _i2, _i3, _i4, _i5);

#define def_check_int_passing12(_i0, _i1, _i2, _i3, _i4, _i5, _i6, _i7, _i8, _i9, _i10, _i11, _func1, _func2, TYPE) \
  values_ ## TYPE .i0 = _i0; \
  values_ ## TYPE .i1 = _i1; \
  values_ ## TYPE .i2 = _i2; \
  values_ ## TYPE .i3 = _i3; \
  values_ ## TYPE .i4 = _i4; \
  values_ ## TYPE .i5 = _i5; \
  values_ ## TYPE .i6 = _i6; \
  values_ ## TYPE .i7 = _i7; \
  values_ ## TYPE .i8 = _i8; \
  values_ ## TYPE .i9 = _i9; \
  values_ ## TYPE .i10 = _i10; \
  values_ ## TYPE .i11 = _i11; \
  WRAP_CALL(_func1) (_i0, _i1, _i2, _i3, _i4, _i5, _i6, _i7, _i8, _i9, _i10, _i11); \
  \
  clear_int_registers; \
  iregs.I0 = _i0; \
  iregs.I1 = _i1; \
  iregs.I2 = _i2; \
  num_iregs = 3; \
  WRAP_CALL(_func2) (_i0, _i1, _i2, _i3, _i4, _i5, _i6, _i7, _i8, _i9, _i10, _i11);

void
test_ints_on_stack ()
{
  def_check_int_passing6(32, 33, 34, 35, 36, 37, fun_check_int_passing_int6_values, fun_check_int_passing_int6_regs, int);
}

void
test_too_many_ints ()
{
  def_check_int_passing12(32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, fun_check_int_passing_int12_values, fun_check_int_passing_int12_regs, int);
}

void
test_longs_on_stack ()
{
  def_check_int_passing6(32, 33, 34, 35, 36, 37, fun_check_int_passing_long6_values, fun_check_int_passing_long6_regs, long);
}

void
test_too_many_longs ()
{
  def_check_int_passing12(32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, fun_check_int_passing_long12_values, fun_check_int_passing_long12_regs, long);
}

int
main (void)
{
  test_ints_on_stack ();
  test_too_many_ints ();
  test_longs_on_stack ();
  test_too_many_longs ();
  return 0;
}
