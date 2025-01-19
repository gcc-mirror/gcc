/* { dg-do compile } */
/* { dg-require-effective-target rv64 } */
/* { dg-options "-march=rv64gc_zicbom_zicbop_zicboz -mabi=lp64d -std=gnu17" } */

#include "riscv_cmo.h"

void foo1 (void *addr)
{
    __riscv_cmo_clean(0);
    __riscv_cmo_clean(addr);
    __riscv_cmo_clean((void*)0x111);
}

void foo2 (void *addr)
{
    __riscv_cmo_flush(0);
    __riscv_cmo_flush(addr);
    __riscv_cmo_flush((void*)0x111);
}

void foo3 (void *addr)
{
    __riscv_cmo_inval(0);
    __riscv_cmo_inval(addr);
    __riscv_cmo_inval((void*)0x111);
}

void foo4 (void *addr)
{
    __riscv_cmo_prefetch(addr,0,0);
    __riscv_cmo_prefetch(addr,0,1);
    __riscv_cmo_prefetch(addr,0,2);
    __riscv_cmo_prefetch(addr,0,3);
    __riscv_cmo_prefetch(addr,1,0);
    __riscv_cmo_prefetch(addr,1,1);
    __riscv_cmo_prefetch(addr,1,2);
    __riscv_cmo_prefetch(addr,1,3);
}

int foo5 (int num)
{
    return __riscv_cmo_prefetchi(num);
}

void foo6 (void *addr)
{
    __riscv_cmo_zero(0);
    __riscv_cmo_zero(addr);
    __riscv_cmo_zero((void*)0x121);
}

/* { dg-final { scan-assembler-times "cbo.clean\t" 3 } } */
/* { dg-final { scan-assembler-times "cbo.flush\t" 3 } } */
/* { dg-final { scan-assembler-times "cbo.inval\t" 3 } } */
/* { dg-final { scan-assembler-times "prefetch.r\t" 4 } } */
/* { dg-final { scan-assembler-times "prefetch.w\t" 4 } } */
/* { dg-final { scan-assembler-times "prefetch.i\t" 1 } } */
/* { dg-final { scan-assembler-times "cbo.zero\t" 3 } } */
