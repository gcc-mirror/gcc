/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=hard" } { "" } } */
/* { dg-additional-options "-march=armv8.1-m.main+mve -mfloat-abi=soft -mthumb -mfpu=auto --save-temps" } */

int
foo1 (int value)
{
  int b = value;
  return b;
}

/* { dg-final { scan-assembler "\.fpu softvfp" }  } */
