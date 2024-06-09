/* { dg-do compile } */
/* { dg-require-effective-target cv_simd } */
/* { dg-options "-march=rv32i_xcvsimd -mabi=ilp32" } */

int
foo1 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_cplxmul_i(a, b, c, 0);
}

/* { dg-final { scan-assembler-times "cv\\.cplxmul\\.i" 1 } } */
