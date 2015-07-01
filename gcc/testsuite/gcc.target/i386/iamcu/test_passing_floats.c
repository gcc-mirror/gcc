#include "defines.h"
#include "macros.h"
#include "args.h"

struct IntegerRegisters iregbits = { ~0, ~0, ~0, ~0, ~0, ~0 };
struct IntegerRegisters iregs;
unsigned int num_iregs;

/* This struct holds values for argument checking.  */
struct
{
  float f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22, f23;
} values_float;

struct
{
  double f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22, f23;
} values_double;

struct
{
  ldouble f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22, f23;
} values_ldouble;

void
fun_check_float_passing_float8_values (float f0 ATTRIBUTE_UNUSED, float f1 ATTRIBUTE_UNUSED, float f2 ATTRIBUTE_UNUSED, float f3 ATTRIBUTE_UNUSED, float f4 ATTRIBUTE_UNUSED, float f5 ATTRIBUTE_UNUSED, float f6 ATTRIBUTE_UNUSED, float f7 ATTRIBUTE_UNUSED)
{
  /* Check argument values.  */
  assert (values_float.f0 == f0);
  assert (values_float.f1 == f1);
  assert (values_float.f2 == f2);
  assert (values_float.f3 == f3);
  assert (values_float.f4 == f4);
  assert (values_float.f5 == f5);
  assert (values_float.f6 == f6);
  assert (values_float.f7 == f7);

}

void
fun_check_float_passing_float8_regs (float f0 ATTRIBUTE_UNUSED, float f1 ATTRIBUTE_UNUSED, float f2 ATTRIBUTE_UNUSED, float f3 ATTRIBUTE_UNUSED, float f4 ATTRIBUTE_UNUSED, float f5 ATTRIBUTE_UNUSED, float f6 ATTRIBUTE_UNUSED, float f7 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_float_arguments;
}

void
fun_check_float_passing_float16_values (float f0 ATTRIBUTE_UNUSED, float f1 ATTRIBUTE_UNUSED, float f2 ATTRIBUTE_UNUSED, float f3 ATTRIBUTE_UNUSED, float f4 ATTRIBUTE_UNUSED, float f5 ATTRIBUTE_UNUSED, float f6 ATTRIBUTE_UNUSED, float f7 ATTRIBUTE_UNUSED, float f8 ATTRIBUTE_UNUSED, float f9 ATTRIBUTE_UNUSED, float f10 ATTRIBUTE_UNUSED, float f11 ATTRIBUTE_UNUSED, float f12 ATTRIBUTE_UNUSED, float f13 ATTRIBUTE_UNUSED, float f14 ATTRIBUTE_UNUSED, float f15 ATTRIBUTE_UNUSED)
{
  /* Check argument values.  */
  assert (values_float.f0 == f0);
  assert (values_float.f1 == f1);
  assert (values_float.f2 == f2);
  assert (values_float.f3 == f3);
  assert (values_float.f4 == f4);
  assert (values_float.f5 == f5);
  assert (values_float.f6 == f6);
  assert (values_float.f7 == f7);
  assert (values_float.f8 == f8);
  assert (values_float.f9 == f9);
  assert (values_float.f10 == f10);
  assert (values_float.f11 == f11);
  assert (values_float.f12 == f12);
  assert (values_float.f13 == f13);
  assert (values_float.f14 == f14);
  assert (values_float.f15 == f15);

}

void
fun_check_float_passing_float16_regs (float f0 ATTRIBUTE_UNUSED, float f1 ATTRIBUTE_UNUSED, float f2 ATTRIBUTE_UNUSED, float f3 ATTRIBUTE_UNUSED, float f4 ATTRIBUTE_UNUSED, float f5 ATTRIBUTE_UNUSED, float f6 ATTRIBUTE_UNUSED, float f7 ATTRIBUTE_UNUSED, float f8 ATTRIBUTE_UNUSED, float f9 ATTRIBUTE_UNUSED, float f10 ATTRIBUTE_UNUSED, float f11 ATTRIBUTE_UNUSED, float f12 ATTRIBUTE_UNUSED, float f13 ATTRIBUTE_UNUSED, float f14 ATTRIBUTE_UNUSED, float f15 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_float_arguments;
}

void
fun_check_float_passing_float20_values (float f0 ATTRIBUTE_UNUSED, float f1 ATTRIBUTE_UNUSED, float f2 ATTRIBUTE_UNUSED, float f3 ATTRIBUTE_UNUSED, float f4 ATTRIBUTE_UNUSED, float f5 ATTRIBUTE_UNUSED, float f6 ATTRIBUTE_UNUSED, float f7 ATTRIBUTE_UNUSED, float f8 ATTRIBUTE_UNUSED, float f9 ATTRIBUTE_UNUSED, float f10 ATTRIBUTE_UNUSED, float f11 ATTRIBUTE_UNUSED, float f12 ATTRIBUTE_UNUSED, float f13 ATTRIBUTE_UNUSED, float f14 ATTRIBUTE_UNUSED, float f15 ATTRIBUTE_UNUSED, float f16 ATTRIBUTE_UNUSED, float f17 ATTRIBUTE_UNUSED, float f18 ATTRIBUTE_UNUSED, float f19 ATTRIBUTE_UNUSED)
{
  /* Check argument values.  */
  assert (values_float.f0 == f0);
  assert (values_float.f1 == f1);
  assert (values_float.f2 == f2);
  assert (values_float.f3 == f3);
  assert (values_float.f4 == f4);
  assert (values_float.f5 == f5);
  assert (values_float.f6 == f6);
  assert (values_float.f7 == f7);
  assert (values_float.f8 == f8);
  assert (values_float.f9 == f9);
  assert (values_float.f10 == f10);
  assert (values_float.f11 == f11);
  assert (values_float.f12 == f12);
  assert (values_float.f13 == f13);
  assert (values_float.f14 == f14);
  assert (values_float.f15 == f15);
  assert (values_float.f16 == f16);
  assert (values_float.f17 == f17);
  assert (values_float.f18 == f18);
  assert (values_float.f19 == f19);

}

void
fun_check_float_passing_float20_regs (float f0 ATTRIBUTE_UNUSED, float f1 ATTRIBUTE_UNUSED, float f2 ATTRIBUTE_UNUSED, float f3 ATTRIBUTE_UNUSED, float f4 ATTRIBUTE_UNUSED, float f5 ATTRIBUTE_UNUSED, float f6 ATTRIBUTE_UNUSED, float f7 ATTRIBUTE_UNUSED, float f8 ATTRIBUTE_UNUSED, float f9 ATTRIBUTE_UNUSED, float f10 ATTRIBUTE_UNUSED, float f11 ATTRIBUTE_UNUSED, float f12 ATTRIBUTE_UNUSED, float f13 ATTRIBUTE_UNUSED, float f14 ATTRIBUTE_UNUSED, float f15 ATTRIBUTE_UNUSED, float f16 ATTRIBUTE_UNUSED, float f17 ATTRIBUTE_UNUSED, float f18 ATTRIBUTE_UNUSED, float f19 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_float_arguments;
}

void
fun_check_float_passing_double8_values (double f0 ATTRIBUTE_UNUSED, double f1 ATTRIBUTE_UNUSED, double f2 ATTRIBUTE_UNUSED, double f3 ATTRIBUTE_UNUSED, double f4 ATTRIBUTE_UNUSED, double f5 ATTRIBUTE_UNUSED, double f6 ATTRIBUTE_UNUSED, double f7 ATTRIBUTE_UNUSED)
{
  /* Check argument values.  */
  assert (values_double.f0 == f0);
  assert (values_double.f1 == f1);
  assert (values_double.f2 == f2);
  assert (values_double.f3 == f3);
  assert (values_double.f4 == f4);
  assert (values_double.f5 == f5);
  assert (values_double.f6 == f6);
  assert (values_double.f7 == f7);

}

void
fun_check_float_passing_double8_regs (double f0 ATTRIBUTE_UNUSED, double f1 ATTRIBUTE_UNUSED, double f2 ATTRIBUTE_UNUSED, double f3 ATTRIBUTE_UNUSED, double f4 ATTRIBUTE_UNUSED, double f5 ATTRIBUTE_UNUSED, double f6 ATTRIBUTE_UNUSED, double f7 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_double_arguments;
}

void
fun_check_float_passing_double16_values (double f0 ATTRIBUTE_UNUSED, double f1 ATTRIBUTE_UNUSED, double f2 ATTRIBUTE_UNUSED, double f3 ATTRIBUTE_UNUSED, double f4 ATTRIBUTE_UNUSED, double f5 ATTRIBUTE_UNUSED, double f6 ATTRIBUTE_UNUSED, double f7 ATTRIBUTE_UNUSED, double f8 ATTRIBUTE_UNUSED, double f9 ATTRIBUTE_UNUSED, double f10 ATTRIBUTE_UNUSED, double f11 ATTRIBUTE_UNUSED, double f12 ATTRIBUTE_UNUSED, double f13 ATTRIBUTE_UNUSED, double f14 ATTRIBUTE_UNUSED, double f15 ATTRIBUTE_UNUSED)
{
  /* Check argument values.  */
  assert (values_double.f0 == f0);
  assert (values_double.f1 == f1);
  assert (values_double.f2 == f2);
  assert (values_double.f3 == f3);
  assert (values_double.f4 == f4);
  assert (values_double.f5 == f5);
  assert (values_double.f6 == f6);
  assert (values_double.f7 == f7);
  assert (values_double.f8 == f8);
  assert (values_double.f9 == f9);
  assert (values_double.f10 == f10);
  assert (values_double.f11 == f11);
  assert (values_double.f12 == f12);
  assert (values_double.f13 == f13);
  assert (values_double.f14 == f14);
  assert (values_double.f15 == f15);

}

void
fun_check_float_passing_double16_regs (double f0 ATTRIBUTE_UNUSED, double f1 ATTRIBUTE_UNUSED, double f2 ATTRIBUTE_UNUSED, double f3 ATTRIBUTE_UNUSED, double f4 ATTRIBUTE_UNUSED, double f5 ATTRIBUTE_UNUSED, double f6 ATTRIBUTE_UNUSED, double f7 ATTRIBUTE_UNUSED, double f8 ATTRIBUTE_UNUSED, double f9 ATTRIBUTE_UNUSED, double f10 ATTRIBUTE_UNUSED, double f11 ATTRIBUTE_UNUSED, double f12 ATTRIBUTE_UNUSED, double f13 ATTRIBUTE_UNUSED, double f14 ATTRIBUTE_UNUSED, double f15 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_double_arguments;
}

void
fun_check_float_passing_double20_values (double f0 ATTRIBUTE_UNUSED, double f1 ATTRIBUTE_UNUSED, double f2 ATTRIBUTE_UNUSED, double f3 ATTRIBUTE_UNUSED, double f4 ATTRIBUTE_UNUSED, double f5 ATTRIBUTE_UNUSED, double f6 ATTRIBUTE_UNUSED, double f7 ATTRIBUTE_UNUSED, double f8 ATTRIBUTE_UNUSED, double f9 ATTRIBUTE_UNUSED, double f10 ATTRIBUTE_UNUSED, double f11 ATTRIBUTE_UNUSED, double f12 ATTRIBUTE_UNUSED, double f13 ATTRIBUTE_UNUSED, double f14 ATTRIBUTE_UNUSED, double f15 ATTRIBUTE_UNUSED, double f16 ATTRIBUTE_UNUSED, double f17 ATTRIBUTE_UNUSED, double f18 ATTRIBUTE_UNUSED, double f19 ATTRIBUTE_UNUSED)
{
  /* Check argument values.  */
  assert (values_double.f0 == f0);
  assert (values_double.f1 == f1);
  assert (values_double.f2 == f2);
  assert (values_double.f3 == f3);
  assert (values_double.f4 == f4);
  assert (values_double.f5 == f5);
  assert (values_double.f6 == f6);
  assert (values_double.f7 == f7);
  assert (values_double.f8 == f8);
  assert (values_double.f9 == f9);
  assert (values_double.f10 == f10);
  assert (values_double.f11 == f11);
  assert (values_double.f12 == f12);
  assert (values_double.f13 == f13);
  assert (values_double.f14 == f14);
  assert (values_double.f15 == f15);
  assert (values_double.f16 == f16);
  assert (values_double.f17 == f17);
  assert (values_double.f18 == f18);
  assert (values_double.f19 == f19);

}

void
fun_check_float_passing_double20_regs (double f0 ATTRIBUTE_UNUSED, double f1 ATTRIBUTE_UNUSED, double f2 ATTRIBUTE_UNUSED, double f3 ATTRIBUTE_UNUSED, double f4 ATTRIBUTE_UNUSED, double f5 ATTRIBUTE_UNUSED, double f6 ATTRIBUTE_UNUSED, double f7 ATTRIBUTE_UNUSED, double f8 ATTRIBUTE_UNUSED, double f9 ATTRIBUTE_UNUSED, double f10 ATTRIBUTE_UNUSED, double f11 ATTRIBUTE_UNUSED, double f12 ATTRIBUTE_UNUSED, double f13 ATTRIBUTE_UNUSED, double f14 ATTRIBUTE_UNUSED, double f15 ATTRIBUTE_UNUSED, double f16 ATTRIBUTE_UNUSED, double f17 ATTRIBUTE_UNUSED, double f18 ATTRIBUTE_UNUSED, double f19 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_double_arguments;
}

void
fun_check_x87_passing_ldouble8_values (ldouble f0 ATTRIBUTE_UNUSED, ldouble f1 ATTRIBUTE_UNUSED, ldouble f2 ATTRIBUTE_UNUSED, ldouble f3 ATTRIBUTE_UNUSED, ldouble f4 ATTRIBUTE_UNUSED, ldouble f5 ATTRIBUTE_UNUSED, ldouble f6 ATTRIBUTE_UNUSED, ldouble f7 ATTRIBUTE_UNUSED)
{
  /* Check argument values.  */
  assert (values_ldouble.f0 == f0);
  assert (values_ldouble.f1 == f1);
  assert (values_ldouble.f2 == f2);
  assert (values_ldouble.f3 == f3);
  assert (values_ldouble.f4 == f4);
  assert (values_ldouble.f5 == f5);
  assert (values_ldouble.f6 == f6);
  assert (values_ldouble.f7 == f7);

}

void
fun_check_x87_passing_ldouble8_regs (ldouble f0 ATTRIBUTE_UNUSED, ldouble f1 ATTRIBUTE_UNUSED, ldouble f2 ATTRIBUTE_UNUSED, ldouble f3 ATTRIBUTE_UNUSED, ldouble f4 ATTRIBUTE_UNUSED, ldouble f5 ATTRIBUTE_UNUSED, ldouble f6 ATTRIBUTE_UNUSED, ldouble f7 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_ldouble_arguments;
}

void
fun_check_x87_passing_ldouble16_values (ldouble f0 ATTRIBUTE_UNUSED, ldouble f1 ATTRIBUTE_UNUSED, ldouble f2 ATTRIBUTE_UNUSED, ldouble f3 ATTRIBUTE_UNUSED, ldouble f4 ATTRIBUTE_UNUSED, ldouble f5 ATTRIBUTE_UNUSED, ldouble f6 ATTRIBUTE_UNUSED, ldouble f7 ATTRIBUTE_UNUSED, ldouble f8 ATTRIBUTE_UNUSED, ldouble f9 ATTRIBUTE_UNUSED, ldouble f10 ATTRIBUTE_UNUSED, ldouble f11 ATTRIBUTE_UNUSED, ldouble f12 ATTRIBUTE_UNUSED, ldouble f13 ATTRIBUTE_UNUSED, ldouble f14 ATTRIBUTE_UNUSED, ldouble f15 ATTRIBUTE_UNUSED)
{
  /* Check argument values.  */
  assert (values_ldouble.f0 == f0);
  assert (values_ldouble.f1 == f1);
  assert (values_ldouble.f2 == f2);
  assert (values_ldouble.f3 == f3);
  assert (values_ldouble.f4 == f4);
  assert (values_ldouble.f5 == f5);
  assert (values_ldouble.f6 == f6);
  assert (values_ldouble.f7 == f7);
  assert (values_ldouble.f8 == f8);
  assert (values_ldouble.f9 == f9);
  assert (values_ldouble.f10 == f10);
  assert (values_ldouble.f11 == f11);
  assert (values_ldouble.f12 == f12);
  assert (values_ldouble.f13 == f13);
  assert (values_ldouble.f14 == f14);
  assert (values_ldouble.f15 == f15);

}

void
fun_check_x87_passing_ldouble16_regs (ldouble f0 ATTRIBUTE_UNUSED, ldouble f1 ATTRIBUTE_UNUSED, ldouble f2 ATTRIBUTE_UNUSED, ldouble f3 ATTRIBUTE_UNUSED, ldouble f4 ATTRIBUTE_UNUSED, ldouble f5 ATTRIBUTE_UNUSED, ldouble f6 ATTRIBUTE_UNUSED, ldouble f7 ATTRIBUTE_UNUSED, ldouble f8 ATTRIBUTE_UNUSED, ldouble f9 ATTRIBUTE_UNUSED, ldouble f10 ATTRIBUTE_UNUSED, ldouble f11 ATTRIBUTE_UNUSED, ldouble f12 ATTRIBUTE_UNUSED, ldouble f13 ATTRIBUTE_UNUSED, ldouble f14 ATTRIBUTE_UNUSED, ldouble f15 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_ldouble_arguments;
}

void
fun_check_x87_passing_ldouble20_values (ldouble f0 ATTRIBUTE_UNUSED, ldouble f1 ATTRIBUTE_UNUSED, ldouble f2 ATTRIBUTE_UNUSED, ldouble f3 ATTRIBUTE_UNUSED, ldouble f4 ATTRIBUTE_UNUSED, ldouble f5 ATTRIBUTE_UNUSED, ldouble f6 ATTRIBUTE_UNUSED, ldouble f7 ATTRIBUTE_UNUSED, ldouble f8 ATTRIBUTE_UNUSED, ldouble f9 ATTRIBUTE_UNUSED, ldouble f10 ATTRIBUTE_UNUSED, ldouble f11 ATTRIBUTE_UNUSED, ldouble f12 ATTRIBUTE_UNUSED, ldouble f13 ATTRIBUTE_UNUSED, ldouble f14 ATTRIBUTE_UNUSED, ldouble f15 ATTRIBUTE_UNUSED, ldouble f16 ATTRIBUTE_UNUSED, ldouble f17 ATTRIBUTE_UNUSED, ldouble f18 ATTRIBUTE_UNUSED, ldouble f19 ATTRIBUTE_UNUSED)
{
  /* Check argument values.  */
  assert (values_ldouble.f0 == f0);
  assert (values_ldouble.f1 == f1);
  assert (values_ldouble.f2 == f2);
  assert (values_ldouble.f3 == f3);
  assert (values_ldouble.f4 == f4);
  assert (values_ldouble.f5 == f5);
  assert (values_ldouble.f6 == f6);
  assert (values_ldouble.f7 == f7);
  assert (values_ldouble.f8 == f8);
  assert (values_ldouble.f9 == f9);
  assert (values_ldouble.f10 == f10);
  assert (values_ldouble.f11 == f11);
  assert (values_ldouble.f12 == f12);
  assert (values_ldouble.f13 == f13);
  assert (values_ldouble.f14 == f14);
  assert (values_ldouble.f15 == f15);
  assert (values_ldouble.f16 == f16);
  assert (values_ldouble.f17 == f17);
  assert (values_ldouble.f18 == f18);
  assert (values_ldouble.f19 == f19);

}

void
fun_check_x87_passing_ldouble20_regs (ldouble f0 ATTRIBUTE_UNUSED, ldouble f1 ATTRIBUTE_UNUSED, ldouble f2 ATTRIBUTE_UNUSED, ldouble f3 ATTRIBUTE_UNUSED, ldouble f4 ATTRIBUTE_UNUSED, ldouble f5 ATTRIBUTE_UNUSED, ldouble f6 ATTRIBUTE_UNUSED, ldouble f7 ATTRIBUTE_UNUSED, ldouble f8 ATTRIBUTE_UNUSED, ldouble f9 ATTRIBUTE_UNUSED, ldouble f10 ATTRIBUTE_UNUSED, ldouble f11 ATTRIBUTE_UNUSED, ldouble f12 ATTRIBUTE_UNUSED, ldouble f13 ATTRIBUTE_UNUSED, ldouble f14 ATTRIBUTE_UNUSED, ldouble f15 ATTRIBUTE_UNUSED, ldouble f16 ATTRIBUTE_UNUSED, ldouble f17 ATTRIBUTE_UNUSED, ldouble f18 ATTRIBUTE_UNUSED, ldouble f19 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_ldouble_arguments;
}

#define def_check_float_passing8(_f0, _f1, _f2, _f3, _f4, _f5, _f6, _f7, _func1, _func2, TYPE) \
  values_ ## TYPE .f0 = _f0; \
  values_ ## TYPE .f1 = _f1; \
  values_ ## TYPE .f2 = _f2; \
  values_ ## TYPE .f3 = _f3; \
  values_ ## TYPE .f4 = _f4; \
  values_ ## TYPE .f5 = _f5; \
  values_ ## TYPE .f6 = _f6; \
  values_ ## TYPE .f7 = _f7; \
  WRAP_CALL(_func1) (_f0, _f1, _f2, _f3, _f4, _f5, _f6, _f7); \
  \
  clear_int_registers; \
  if (sizeof (TYPE) == 4) \
    { \
      u.f = _f0; \
      iregs.I0 = u.i[0]; \
      u.f = _f1; \
      iregs.I1 = u.i[0]; \
      u.f = _f2; \
      iregs.I2 = u.i[0]; \
      num_iregs = 3; \
    } \
  else \
    { \
      u.d = _f0; \
      iregs.I0 = u.i[0]; \
      iregs.I1 = u.i[1]; \
      num_iregs = 2; \
    } \
  WRAP_CALL(_func2) (_f0, _f1, _f2, _f3, _f4, _f5, _f6, _f7);

#define def_check_float_passing16(_f0, _f1, _f2, _f3, _f4, _f5, _f6, _f7, _f8, _f9, _f10, _f11, _f12, _f13, _f14, _f15, _func1, _func2, TYPE) \
  values_ ## TYPE .f0 = _f0; \
  values_ ## TYPE .f1 = _f1; \
  values_ ## TYPE .f2 = _f2; \
  values_ ## TYPE .f3 = _f3; \
  values_ ## TYPE .f4 = _f4; \
  values_ ## TYPE .f5 = _f5; \
  values_ ## TYPE .f6 = _f6; \
  values_ ## TYPE .f7 = _f7; \
  values_ ## TYPE .f8 = _f8; \
  values_ ## TYPE .f9 = _f9; \
  values_ ## TYPE .f10 = _f10; \
  values_ ## TYPE .f11 = _f11; \
  values_ ## TYPE .f12 = _f12; \
  values_ ## TYPE .f13 = _f13; \
  values_ ## TYPE .f14 = _f14; \
  values_ ## TYPE .f15 = _f15; \
  WRAP_CALL(_func1) (_f0, _f1, _f2, _f3, _f4, _f5, _f6, _f7, _f8, _f9, _f10, _f11, _f12, _f13, _f14, _f15); \
  \
  clear_int_registers; \
  if (sizeof (TYPE) == 4) \
    { \
      u.f = _f0; \
      iregs.I0 = u.i[0]; \
      u.f = _f1; \
      iregs.I1 = u.i[0]; \
      u.f = _f2; \
      iregs.I2 = u.i[0]; \
      num_iregs = 3; \
    } \
  else \
    { \
      u.d = _f0; \
      iregs.I0 = u.i[0]; \
      iregs.I1 = u.i[1]; \
      num_iregs = 2; \
    } \
  WRAP_CALL(_func2) (_f0, _f1, _f2, _f3, _f4, _f5, _f6, _f7, _f8, _f9, _f10, _f11, _f12, _f13, _f14, _f15);

#define def_check_float_passing20(_f0, _f1, _f2, _f3, _f4, _f5, _f6, _f7, _f8, _f9, _f10, _f11, _f12, _f13, _f14, _f15, _f16, _f17, _f18, _f19, _func1, _func2, TYPE) \
  values_ ## TYPE .f0 = _f0; \
  values_ ## TYPE .f1 = _f1; \
  values_ ## TYPE .f2 = _f2; \
  values_ ## TYPE .f3 = _f3; \
  values_ ## TYPE .f4 = _f4; \
  values_ ## TYPE .f5 = _f5; \
  values_ ## TYPE .f6 = _f6; \
  values_ ## TYPE .f7 = _f7; \
  values_ ## TYPE .f8 = _f8; \
  values_ ## TYPE .f9 = _f9; \
  values_ ## TYPE .f10 = _f10; \
  values_ ## TYPE .f11 = _f11; \
  values_ ## TYPE .f12 = _f12; \
  values_ ## TYPE .f13 = _f13; \
  values_ ## TYPE .f14 = _f14; \
  values_ ## TYPE .f15 = _f15; \
  values_ ## TYPE .f16 = _f16; \
  values_ ## TYPE .f17 = _f17; \
  values_ ## TYPE .f18 = _f18; \
  values_ ## TYPE .f19 = _f19; \
  WRAP_CALL(_func1) (_f0, _f1, _f2, _f3, _f4, _f5, _f6, _f7, _f8, _f9, _f10, _f11, _f12, _f13, _f14, _f15, _f16, _f17, _f18, _f19); \
  \
  clear_int_registers; \
  if (sizeof (TYPE) == 4) \
    { \
      u.f = _f0; \
      iregs.I0 = u.i[0]; \
      u.f = _f1; \
      iregs.I1 = u.i[0]; \
      u.f = _f2; \
      iregs.I2 = u.i[0]; \
      num_iregs = 3; \
    } \
  else \
    { \
      u.d = _f0; \
      iregs.I0 = u.i[0]; \
      iregs.I1 = u.i[1]; \
      num_iregs = 2; \
    } \
  WRAP_CALL(_func2) (_f0, _f1, _f2, _f3, _f4, _f5, _f6, _f7, _f8, _f9, _f10, _f11, _f12, _f13, _f14, _f15, _f16, _f17, _f18, _f19);

#define def_check_x87_passing8(_f0, _f1, _f2, _f3, _f4, _f5, _f6, _f7, _func1, _func2, TYPE) \
  values_ ## TYPE .f0 = _f0; \
  values_ ## TYPE .f1 = _f1; \
  values_ ## TYPE .f2 = _f2; \
  values_ ## TYPE .f3 = _f3; \
  values_ ## TYPE .f4 = _f4; \
  values_ ## TYPE .f5 = _f5; \
  values_ ## TYPE .f6 = _f6; \
  values_ ## TYPE .f7 = _f7; \
  WRAP_CALL(_func1) (_f0, _f1, _f2, _f3, _f4, _f5, _f6, _f7); \
  \
  clear_int_registers; \
  if (sizeof (TYPE) == 4) \
    { \
      u.f = _f0; \
      iregs.I0 = u.i[0]; \
      u.f = _f1; \
      iregs.I1 = u.i[0]; \
      u.f = _f2; \
      iregs.I2 = u.i[0]; \
      num_iregs = 3; \
    } \
  else \
    { \
      u.d = _f0; \
      iregs.I0 = u.i[0]; \
      iregs.I1 = u.i[1]; \
      num_iregs = 2; \
    } \
  WRAP_CALL(_func2) (_f0, _f1, _f2, _f3, _f4, _f5, _f6, _f7);

#define def_check_x87_passing16(_f0, _f1, _f2, _f3, _f4, _f5, _f6, _f7, _f8, _f9, _f10, _f11, _f12, _f13, _f14, _f15, _func1, _func2, TYPE) \
  values_ ## TYPE .f0 = _f0; \
  values_ ## TYPE .f1 = _f1; \
  values_ ## TYPE .f2 = _f2; \
  values_ ## TYPE .f3 = _f3; \
  values_ ## TYPE .f4 = _f4; \
  values_ ## TYPE .f5 = _f5; \
  values_ ## TYPE .f6 = _f6; \
  values_ ## TYPE .f7 = _f7; \
  values_ ## TYPE .f8 = _f8; \
  values_ ## TYPE .f9 = _f9; \
  values_ ## TYPE .f10 = _f10; \
  values_ ## TYPE .f11 = _f11; \
  values_ ## TYPE .f12 = _f12; \
  values_ ## TYPE .f13 = _f13; \
  values_ ## TYPE .f14 = _f14; \
  values_ ## TYPE .f15 = _f15; \
  WRAP_CALL(_func1) (_f0, _f1, _f2, _f3, _f4, _f5, _f6, _f7, _f8, _f9, _f10, _f11, _f12, _f13, _f14, _f15); \
  \
  clear_int_registers; \
  if (sizeof (TYPE) == 4) \
    { \
      u.f = _f0; \
      iregs.I0 = u.i[0]; \
      u.f = _f1; \
      iregs.I1 = u.i[0]; \
      u.f = _f2; \
      iregs.I2 = u.i[0]; \
      num_iregs = 3; \
    } \
  else \
    { \
      u.d = _f0; \
      iregs.I0 = u.i[0]; \
      iregs.I1 = u.i[1]; \
      num_iregs = 2; \
    } \
  WRAP_CALL(_func2) (_f0, _f1, _f2, _f3, _f4, _f5, _f6, _f7, _f8, _f9, _f10, _f11, _f12, _f13, _f14, _f15);

#define def_check_x87_passing20(_f0, _f1, _f2, _f3, _f4, _f5, _f6, _f7, _f8, _f9, _f10, _f11, _f12, _f13, _f14, _f15, _f16, _f17, _f18, _f19, _func1, _func2, TYPE) \
  values_ ## TYPE .f0 = _f0; \
  values_ ## TYPE .f1 = _f1; \
  values_ ## TYPE .f2 = _f2; \
  values_ ## TYPE .f3 = _f3; \
  values_ ## TYPE .f4 = _f4; \
  values_ ## TYPE .f5 = _f5; \
  values_ ## TYPE .f6 = _f6; \
  values_ ## TYPE .f7 = _f7; \
  values_ ## TYPE .f8 = _f8; \
  values_ ## TYPE .f9 = _f9; \
  values_ ## TYPE .f10 = _f10; \
  values_ ## TYPE .f11 = _f11; \
  values_ ## TYPE .f12 = _f12; \
  values_ ## TYPE .f13 = _f13; \
  values_ ## TYPE .f14 = _f14; \
  values_ ## TYPE .f15 = _f15; \
  values_ ## TYPE .f16 = _f16; \
  values_ ## TYPE .f17 = _f17; \
  values_ ## TYPE .f18 = _f18; \
  values_ ## TYPE .f19 = _f19; \
  WRAP_CALL(_func1) (_f0, _f1, _f2, _f3, _f4, _f5, _f6, _f7, _f8, _f9, _f10, _f11, _f12, _f13, _f14, _f15, _f16, _f17, _f18, _f19); \
  \
  clear_int_registers; \
  if (sizeof (TYPE) == 4) \
    { \
      u.f = _f0; \
      iregs.I0 = u.i[0]; \
      u.f = _f1; \
      iregs.I1 = u.i[0]; \
      u.f = _f2; \
      iregs.I2 = u.i[0]; \
      num_iregs = 3; \
    } \
  else \
    { \
      u.d = _f0; \
      iregs.I0 = u.i[0]; \
      iregs.I1 = u.i[1]; \
      num_iregs = 2; \
    } \
  WRAP_CALL(_func2) (_f0, _f1, _f2, _f3, _f4, _f5, _f6, _f7, _f8, _f9, _f10, _f11, _f12, _f13, _f14, _f15, _f16, _f17, _f18, _f19);

void
test_floats_on_stack ()
{
  union
    {
      float f;
      double d;
      int i[2];
    } u;
  def_check_float_passing8(32, 33, 34, 35, 36, 37, 38, 39, fun_check_float_passing_float8_values, fun_check_float_passing_float8_regs, float);

  def_check_float_passing16(32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, fun_check_float_passing_float16_values, fun_check_float_passing_float16_regs, float);
}

void
test_too_many_floats ()
{
  union
    {
      float f;
      double d;
      int i[2];
    } u;
  def_check_float_passing20(32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, fun_check_float_passing_float20_values, fun_check_float_passing_float20_regs, float);
}

void
test_doubles_on_stack ()
{
  union
    {
      float f;
      double d;
      int i[2];
    } u;
  def_check_float_passing8(32, 33, 34, 35, 36, 37, 38, 39, fun_check_float_passing_double8_values, fun_check_float_passing_double8_regs, double);

  def_check_float_passing16(32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, fun_check_float_passing_double16_values, fun_check_float_passing_double16_regs, double);
}

void
test_too_many_doubles ()
{
  union
    {
      float f;
      double d;
      int i[2];
    } u;
  def_check_float_passing20(32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, fun_check_float_passing_double20_values, fun_check_float_passing_double20_regs, double);
}

void
test_long_doubles_on_stack ()
{
  union
    {
      float f;
      double d;
      int i[2];
    } u;
  def_check_x87_passing8(32, 33, 34, 35, 36, 37, 38, 39, fun_check_x87_passing_ldouble8_values, fun_check_x87_passing_ldouble8_regs, ldouble);
}

void
test_too_many_long_doubles ()
{
  union
    {
      float f;
      double d;
      int i[2];
    } u;
  def_check_x87_passing20(32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, fun_check_x87_passing_ldouble20_values, fun_check_x87_passing_ldouble20_regs, ldouble);
}

void
test_float128s_on_stack ()
{
}

void
test_too_many_float128s ()
{
}


int
main (void)
{
  test_floats_on_stack ();
  test_too_many_floats ();
  test_doubles_on_stack ();
  test_too_many_doubles ();
  test_long_doubles_on_stack ();
  test_too_many_long_doubles ();
  test_float128s_on_stack ();
  test_too_many_float128s ();
  return 0;
}
