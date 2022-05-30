/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zicbom -mabi=lp64" } */

int foo1()
{
    return __builtin_riscv_zicbom_cbo_clean();
}

int foo2()
{
    return __builtin_riscv_zicbom_cbo_flush();
}

int foo3()
{
    return __builtin_riscv_zicbom_cbo_inval();
}

/* { dg-final { scan-assembler-times "cbo.clean" 1 } } */
/* { dg-final { scan-assembler-times "cbo.flush" 1 } } */
/* { dg-final { scan-assembler-times "cbo.inval" 1 } } */
