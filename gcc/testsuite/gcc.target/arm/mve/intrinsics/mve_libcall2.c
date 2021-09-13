/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */
/* { dg-additional-options "-march=armv8.1-m.main+mve -mfloat-abi=hard -mthumb -mfpu=auto -mno-long-calls --save-temps" } */

double
foo (double a, double b, double c)
{
  return a + b + c;
}

/* { dg-final { scan-assembler-times "bl\\t__aeabi_dadd" 2 } } */

double
foo1 (double a, double b, double c)
{
  return a - b - c;
}

/* { dg-final { scan-assembler-times "bl\\t__aeabi_dsub" 2 } } */

double
foo2 (double a, double b, double c)
{
  return a * b * c;
}

/* { dg-final { scan-assembler-times "bl\\t__aeabi_dmul" 2 } } */

double
foo3 (double b, double c)
{
  return b / c;
}

/* { dg-final { scan-assembler "bl\\t__aeabi_ddiv" }  } */

int
foo4 (double b, double c)
{
  return b < c;
}

/* { dg-final { scan-assembler "bl\\t__aeabi_dcmplt" }  } */

int
foo5 (double b, double c)
{
  return b > c;
}

/* { dg-final { scan-assembler "bl\\t__aeabi_dcmpgt" }  } */

int
foo6 (double b, double c)
{
  return b != c;
}

/* { dg-final { scan-assembler "bl\\t__aeabi_dcmpeq" }  } */

int
foo7 (double b, double c)
{
  return b == c;
}

/* { dg-final { scan-assembler-times "bl\\t__aeabi_dcmpeq" 2 } } */
