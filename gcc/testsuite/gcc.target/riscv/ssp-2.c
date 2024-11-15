/* { dg-do compile { target { riscv64*-*-* } } } */
/* { dg-options "-O0 -march=rv64gc_zicfiss -mabi=lp64d -fcf-protection=return" } */

void __attribute__ ((interrupt))
foo (void)
{
}
/* { dg-final { scan-assembler-times "sd\tra" 1 } } */
/* { dg-final { scan-assembler-times "ld\tra" 1 } } */
/* { dg-final { scan-assembler-times "sspopchk\tra" 1 } } */
