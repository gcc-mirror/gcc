/* { dg-do compile } */
/* { dg-require-effective-target cv_elw } */
/* { dg-options "-march=rv32i_xcvelw -mabi=ilp32" } */

int
foo (void* b)
{
    return __builtin_riscv_cv_elw_elw (b + 8);
}

/* { dg-final { scan-assembler-times "cv\\.elw\t\[a-z\]\[0-99\],\[0-99\]\\(\[a-z\]\[0-99\]\\)" 1 } } */
