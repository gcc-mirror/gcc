/* { dg-do compile } */
/* { dg-options "-O2 -fsignaling-nans -march=loongarch64 -mfpu=64 -mabi=lp64d" } */
/* { dg-final { scan-assembler-times "fclass\\.s" 1 } } */
/* { dg-final { scan-assembler-times "fclass\\.d" 1 } } */
/* { dg-final { scan-assembler-not "fcmp" } } */

__attribute__ ((noipa)) int
test_fclass_f (float f)
{
  return __builtin_isinf (f)
	 | __builtin_isnormal (f) << 1
	 | __builtin_isfinite (f) << 2
	 | __builtin_isnan (f) << 3;
}

__attribute__ ((noipa)) int
test_fclass_d (double d)
{
  return __builtin_isinf (d)
	 | __builtin_isnormal (d) << 1
	 | __builtin_isfinite (d) << 2
	 | __builtin_isnan (d) << 3;
}
