/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zkne -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */

#include <stdint-gcc.h>

uint64_t foo1(uint64_t rs1, unsigned rnum)
{
    return __builtin_riscv_aes64ks1i(rs1,-1);	/* { dg-error "invalid argument to built-in function" } */
}

uint64_t foo2(uint64_t rs1, unsigned rnum)
{
    return __builtin_riscv_aes64ks1i(rs1,11);	/* { dg-error "invalid argument to built-in function" } */
}

uint64_t foo3(uint64_t rs1, unsigned rnum)
{
    return __builtin_riscv_aes64ks1i(rs1,rnum);	/* { dg-error "invalid argument to built-in function" } */
}

