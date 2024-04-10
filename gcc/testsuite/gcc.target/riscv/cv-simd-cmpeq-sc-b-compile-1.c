/* { dg-do compile } */
/* { dg-require-effective-target cv_simd } */
/* { dg-options "-march=rv32i_xcvsimd -mabi=ilp32" } */

int
foo1 (int a, int b)
{
	return __builtin_riscv_cv_simd_cmpeq_sc_b(a, b);
}

int
foo2 (int a)
{
	return __builtin_riscv_cv_simd_cmpeq_sc_b(a, -32);
}

int
foo3 (int a)
{
	return __builtin_riscv_cv_simd_cmpeq_sc_b(a, 0);
}

int
foo4 (int a)
{
	return __builtin_riscv_cv_simd_cmpeq_sc_b(a, 31);
}

/* { dg-final { scan-assembler-times "cv\\.cmpeq\\.sc\\.b" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.cmpeq\\.sci\\.b" 3 } } */
