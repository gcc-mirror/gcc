/* { dg-do compile { target { riscv64*-*-* } } } */
/* { dg-options "-march=rv64gc -mabi=lp64" } */
int
sub (unsigned int i, unsigned int j, unsigned int k, int *array)
{
  return array[i] + array[j] + array[k];
}
/* { dg-final { scan-assembler-times {\mslli} 3 } } */
