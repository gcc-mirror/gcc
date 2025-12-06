/* { dg-do assemble } */
/* { dg-options "-O2 -mcmodel=medany -save-temps" } */

int a[1];

__UINTPTR_TYPE__
foo(void)
{
  return (__UINTPTR_TYPE__)a + 0x7fffffff;
}

__UINTPTR_TYPE__
bfoo(void)
{
  return (__UINTPTR_TYPE__)a + 0x40000000;
}

__UINTPTR_TYPE__
sfoo(void)
{
  return (__UINTPTR_TYPE__)a + 0x3fffffff;
}

__UINTPTR_TYPE__
bar(void)
{
  return (__UINTPTR_TYPE__)a - 0x80000000;
}

__UINTPTR_TYPE__
bbar(void)
{
  return (__UINTPTR_TYPE__)a - 0x40000000;
}

__UINTPTR_TYPE__
sbar(void)
{
  return (__UINTPTR_TYPE__)a - 0x3fffffff;
}

/* /* dg-final { scan-assembler-times "lla\ta[0-9]*, a$" 4 { target riscv64-*-* } } } */
/* /* dg-final { scan-assembler-times "lla\ta[0-9]*, a[-+]" 2 { target riscv64-*-* } } } */

/* /* dg-final { scan-assembler-times "lla\ta[0-9]*, a[-+]$" 6 { target riscv32-*-* } } } */
/* /* dg-final { scan-assembler-not "lla\ta[0-9]*, a$" { target riscv32-*-* } } } */
