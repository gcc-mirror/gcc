/* { dg-do compile } */
/* { dg-require-effective-target cv_simd } */
/* { dg-options "-march=rv32i_xcvsimd -mabi=ilp32" } */

int
foo1 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_sdotup_sc_b(a, b, c);
}

int
foo2 (int a, int b)
{
	return __builtin_riscv_cv_simd_sdotup_sc_b(a, 0, b);
}

int
foo3 (int a, int b)
{
	return __builtin_riscv_cv_simd_sdotup_sc_b(a, 63, b);
}

/* { dg-final { scan-assembler-times "cv\\.sdotup\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sdotup\\.sci\\.b" 2 } } */
