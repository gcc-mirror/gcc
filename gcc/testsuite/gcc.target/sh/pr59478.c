/* Verify that the fmac insn is used for the standard fmaf function.  */
/* { dg-do compile { target { any_fpu } } }  */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "fpul" 8 } } */
/* { dg-final { scan-assembler-times "\tflds\tfr" 2 } } */
/* { dg-final { scan-assembler-times "\tsts\tfpul" 2 } } */
/* { dg-final { scan-assembler-times "\tlds\tr" 2 } } */
/* { dg-final { scan-assembler-times "\tfsts\tfpul" 2 } } */
/* { dg-final { scan-assembler-not "mov" } } */

int
test_00 (float val)
{
  char valbytes[sizeof (float)];
  __builtin_memcpy (valbytes, &val, sizeof (float));

  int result;
  __builtin_memcpy (&result, valbytes, sizeof (int));

  return result;
}

float
test_01 (int val)
{
  char valbytes[sizeof (int)];
  __builtin_memcpy (valbytes, &val, sizeof (int));

  float result;
  __builtin_memcpy (&result, valbytes, sizeof (float));

  return result;
}

int
test_02 (float val)
{
  union { int i; float f; } tmp;
  tmp.f = val;
  return tmp.i;
}

float
test_03 (int val)
{
  union { int i; float f; } tmp;
  tmp.i = val;
  return tmp.f;
}
