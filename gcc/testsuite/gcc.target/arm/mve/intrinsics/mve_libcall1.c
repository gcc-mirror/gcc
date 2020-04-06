/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */
/* { dg-additional-options "-march=armv8.1-m.main+mve -mfloat-abi=hard -mthumb -mfpu=auto --save-temps" } */

float
foo (float a, float b, float c)
{
  return a + b + c;
}

/* { dg-final { scan-assembler-times "bl\\t__aeabi_fadd" 2 } } */

float
foo1 (float a, float b, float c)
{
  return a - b - c;
}

/* { dg-final { scan-assembler-times "bl\\t__aeabi_fsub" 2 } } */

float
foo2 (float a, float b, float c)
{
  return a * b * c;
}

/* { dg-final { scan-assembler-times "bl\\t__aeabi_fmul" 2 } } */

float
foo3 (float b, float c)
{
  return b / c;
}

/* { dg-final { scan-assembler "bl\\t__aeabi_fdiv" }  } */

int
foo4 (float b, float c)
{
  return b < c;
}

/* { dg-final { scan-assembler "bl\\t__aeabi_fcmplt" }  } */

int
foo5 (float b, float c)
{
  return b > c;
}

/* { dg-final { scan-assembler "bl\\t__aeabi_fcmpgt" }  } */

int
foo6 (float b, float c)
{
  return b != c;
}

/* { dg-final { scan-assembler "bl\\t__aeabi_fcmpeq" }  } */

int
foo7 (float b, float c)
{
  return b == c;
}

/* { dg-final { scan-assembler-times "bl\\t__aeabi_fcmpeq" 2 } } */
