/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
/* { dg-options "-march=rv32gc" { target { rv32 } } } */
/* { dg-options "-march=rv64gc" { target { rv64 } } } */

__attribute__((norelax))
void foo1()
{}

void foo2(void)
{}

int main()
{
  foo1();
  foo2();
  return 0;
}

/* { dg-final { scan-assembler-times ".option push" 1 } } */
/* { dg-final { scan-assembler-times ".option norelax" 1 } } */
/* { dg-final { scan-assembler-times ".option pop" 1 } } */
