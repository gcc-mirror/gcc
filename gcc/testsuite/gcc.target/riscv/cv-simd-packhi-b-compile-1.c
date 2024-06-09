/* { dg-do compile } */
/* { dg-require-effective-target cv_simd } */
/* { dg-options "-march=rv32i_xcvsimd -mabi=ilp32" } */

int
foo1 (int a, int b, int c)
{
	return __builtin_riscv_cv_simd_packhi_b(a, b, c);
}

/* { dg-final { scan-assembler-times "cv\\.packhi\\.b" 1 } } */
