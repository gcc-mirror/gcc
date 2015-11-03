/* Check that the FPUL register is used when reading a float as an int and
   vice versa, as opposed to pushing and popping the values over the stack.  */
/* { dg-do compile { target { any_fpu } } }  */
/* { dg-options "-O1" } */
/* { dg-final { scan-assembler "fpul" } } */
/* { dg-final { scan-assembler-not "r15" } } */

int
float_as_int (float val)
{
  union { float f; int i; } u;
  u.f = val;
  return u.i;
}

float
int_as_float (int val)
{
  union { float f; int i; } u;
  u.i = val;
  return u.f;
}

