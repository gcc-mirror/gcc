/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

void __attribute__((riscv_vector_cc)) bar1 (int a);
void bar2 ();

void __attribute__((riscv_vector_cc))
foo1 (int a)
{
  bar1 (a);
}

void __attribute__((riscv_vector_cc))
foo2 (int a)
{
  char data[1024];
  bar2 ();
}

void
foo3 (int *a)
{
  bar1 (*a);
}

/* { dg-final { scan-assembler-not {\.variant_cc\tbar2} } } */
/* { dg-final { scan-assembler-not {\.variant_cc\tfoo3} } } */
/* { dg-final { scan-assembler-times {\.variant_cc\tbar1} 1 } } */
/* { dg-final { scan-assembler-times {\.variant_cc\tfoo1} 1 } } */
/* { dg-final { scan-assembler-times {\.variant_cc\tfoo2} 1 } } */
