/* { dg-do compile { target { riscv64*-*-* } } } */
/* { dg-options "-march=rv64gc_zicfilp -mabi=lp64d -fcf-protection=branch" } */
void __attribute__ ((interrupt))
foo (void)
{
}
/* { dg-final { scan-assembler-not "lpad\t" } } */
