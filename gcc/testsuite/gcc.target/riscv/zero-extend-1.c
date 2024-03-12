/* { dg-do compile { target { riscv64*-*-* } } } */
/* { dg-options "-march=rv64gc -mabi=lp64" } */
unsigned long
sub1 (unsigned int i)
{
  return i >> 1;
}
/* { dg-final { scan-assembler-times {\msrliw} 1 } } */
