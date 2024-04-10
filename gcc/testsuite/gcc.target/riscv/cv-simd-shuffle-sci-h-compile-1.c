/* { dg-do compile } */
/* { dg-require-effective-target cv_simd } */
/* { dg-options "-march=rv32i_xcvsimd -mabi=ilp32" } */

int
foo1 (int a)
{
	return __builtin_riscv_cv_simd_shuffle_sci_h(a, 0);
}

/* { dg-final { scan-assembler-times "cv\\.shuffle\\.sci\\.h" 1 } } */
