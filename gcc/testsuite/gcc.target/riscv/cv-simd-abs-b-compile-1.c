/* { dg-do compile } */
/* { dg-require-effective-target cv_simd } */
/* { dg-options "-march=rv32i_xcvsimd -mabi=ilp32" } */

int
foo1 (int a)
{
	return __builtin_riscv_cv_simd_abs_b(a);
}

/* { dg-final { scan-assembler-times "cv\\.abs\\.b" 1 } } */
