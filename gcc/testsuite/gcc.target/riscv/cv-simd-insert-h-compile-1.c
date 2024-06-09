/* { dg-do compile } */
/* { dg-require-effective-target cv_simd } */
/* { dg-options "-march=rv32i_xcvsimd -mabi=ilp32" } */

int
foo1 (int a, int b)
{
	return __builtin_riscv_cv_simd_insert_h (a, b, 0);
}

int
foo2 (int a, int b)
{
	return __builtin_riscv_cv_simd_insert_h (a, b, 1);
}

int
foo3 (int a, int b)
{
	return __builtin_riscv_cv_simd_insert_h (a, b, 255);
}

/* { dg-final { scan-assembler-times "cv\\.insert\\.h" 3 } } */
