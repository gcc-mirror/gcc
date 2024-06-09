/* { dg-do compile } */
/* { dg-require-effective-target cv_simd } */
/* { dg-options "-march=rv32i_xcvsimd -mabi=ilp32" } */

unsigned int
foo1 (unsigned int a)
{
	return __builtin_riscv_cv_simd_shuffle_sci_b (a, 128);
}

unsigned int
foo2 (unsigned int a)
{
        return __builtin_riscv_cv_simd_shuffle_sci_b (a, 191);
}

/* { dg-final { scan-assembler-times "cv\\.shufflei2\\.sci\\.b" 2 } } */
/* { dg-final { scan-assembler-times "cv\\.shufflei2\\.sci\\.b\t\[a-z\]\[0-99\],\[a-z\]\[0-99\],0" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.shufflei2\\.sci\\.b\t\[a-z\]\[0-99\],\[a-z\]\[0-99\],63" 1 } } */
