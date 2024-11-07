/* { dg-do compile } */
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

/* { dg-final { scan-assembler-times ".option push\t" 1 } } */
/* { dg-final { scan-assembler-times ".option norelax\t" 1 } } */
/* { dg-final { scan-assembler-times ".option pop\t" 1 } } */
