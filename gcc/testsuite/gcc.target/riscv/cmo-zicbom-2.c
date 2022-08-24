/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zicbom -mabi=ilp32" } */

int var;

void foo1()
{
    __builtin_riscv_zicbom_cbo_clean(0);
    __builtin_riscv_zicbom_cbo_clean(&var);
    __builtin_riscv_zicbom_cbo_clean((void*)0x111);
}

void foo2()
{
    __builtin_riscv_zicbom_cbo_flush(0);
    __builtin_riscv_zicbom_cbo_flush(&var);
    __builtin_riscv_zicbom_cbo_flush((void*)0x111);
}

void foo3()
{
    __builtin_riscv_zicbom_cbo_inval(0);
    __builtin_riscv_zicbom_cbo_inval(&var);
    __builtin_riscv_zicbom_cbo_inval((void*)0x111);
}

/* { dg-final { scan-assembler-times "cbo.clean" 3 } } */
/* { dg-final { scan-assembler-times "cbo.flush" 3 } } */
/* { dg-final { scan-assembler-times "cbo.inval" 3 } } */
