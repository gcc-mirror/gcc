/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-march=rv64gc -mabi=lp64d  -ftrapping-math" { target { rv64 } } } */
/* { dg-options "-march=rv32gc -mabi=ilp32d -ftrapping-math" { target { rv32 } } } */

int d_isfinite(double a)
{
  return __builtin_isfinite(a);
}

int d_isnormal(double a)
{
  return __builtin_isnormal(a);
}

int d_isinf(double a)
{
  return __builtin_isinf(a);
}

int f_isfinite(float a)
{
  return __builtin_isfinite(a);
}

int f_isnormal(float a)
{
  return __builtin_isnormal(a);
}

int f_isinf(float a)
{
  return __builtin_isinf(a);
}

/* { dg-final { scan-assembler-not   {\mfrflags}  } } */
/* { dg-final { scan-assembler-not   {\mfsflags}  } } */
/* { dg-final { scan-assembler-times {\tfclass} 6 } } */
